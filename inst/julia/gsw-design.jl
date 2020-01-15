# ==============================================================================
# gsw-design.jl -- Gram-Schmidt walk design
#
# An efficient implementation for sampling assignments from the Gram--Schmidt Walk Design.
#
# Copyright (C) 2020
# Christopher Harshaw, Fredrik Savje, Daniel Spielman & Peng Zhang
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/
# ==============================================================================

function sample_gs_walk(X; lam=0.5, treatment_probs = 0.5*ones(size(X,1)), balanced=false, num_samples=1)
    """
    # sample_gs_walk
    # An fast implementation for sampling from the Gram-Schmidt Walk Design.
    # Maintains a cholesky factorization of (I + X * X^T ) for faster repeated linear system solves
    # and has a recursive component for more effective memory allocation.
    #
    # The covariate matrix is automatically scaled so max covariate norm is 1.
    #
    # Input
    # 	X               an n by d matrix which has the covariate vectors x_1, x_2 ... x_n as rows
    #   lam           a real value in (0,1) specifying weight
    #   treatment_probs an n length vector of treatment probabilities, entries in [0,1]
    #   balanced        set `true` to run the balanced GSW; otherwise, leave false
    #   num_samples     the number of samples to draw
    #
    # Output
    #   assignment_list     the sampled 0/1 assignment vectors, (num_samples,n) integer array
    """

    # transpose the covariate matrix so it has covariates as columns (this is a quick fix)
    Xt = X'

    # get the dimensions, re-scale so covariate norm is equal to 1
    d, n = size(Xt)
    max_norm = maximum([norm(Xt[:,i]) for i=1:n])
    if max_norm > eps()
        Xt ./= max_norm
    end

    # pre-processing: compute cholesky factorization of I + (1-a) X X^T
    M = (lam / (1-lam)) * I +  (Xt * Xt')
    MC = cholesky(M)

    # compute sum of covariances if necessary
    if balanced
        cov_sum = sum(Xt, dims=2)
    else
        cov_sum = nothing
    end

    # transform treatment probabilities to means (starting position of walk)
    z0 = (2.0 * treatment_probs) .- 1.0

    # sample the num_sample assignments
    assignment_list = zeros(Int64, num_samples, n)
    for i=1:num_samples

        # run the recursive version of the walk
        z = _gs_walk_recur(Xt, copy(MC), copy(z0), lam, balanced, cov_sum)

        # transform z from +/- 1 to an assignment vector in 0/1
        for j=1:n
            assignment_list[i,j] = (z[j] < 0) ? 0 : 1
        end
    end
    return assignment_list
end

function _gs_walk_recur(X, MC, z, lam, balanced, cov_sum)
    """
    # _gs_walk_recur
    # Carries out the iterative procedure of the Gram--Schmidt walk.
    # This function recusively cals itself after sufficiently many variables have been frozen to achieve
    # better memory allocation. The cholesky factorization of (I + X * X^T ) is also maintained.
    #
    # Input
    # 	X           an d by n matrix which has the covariate vectors x_1, x_2 ... x_n as columns
    #   MC          the relevant cholesky factorization
    #   z           the starting point of the walk. This value is modified in place.
    #   lam           a real value in (0,1) specifying weight
    #   balanced        set `true` to run the balanced GSW; otherwise, leave false
    #
    # Output
    #   z               the random +/- 1 vector, length n array
    """

    # get the dimensions, set tolerance
    d, n = size(X)
    tol = 100*eps()

    # initialize alive variablesa and pivot index
    live_not_pivot = trues(n) # bit array, space efficient
    p_alive = true

    # select pivot, update cholesky and covariate sum
    p = rand(1:n)
    live_not_pivot[p] = false
    lowrankdowndate!(MC, X[:,p])
    if balanced
        cov_sum -= X[:,p]
    end

    # iterate through the GS walk
    iter = 1

    # will recurse if freeze a large number of variables - need to optimize those parameters.
    num_frozen = 0
    targ_frozen = max(5, div(n,3))

    while any(live_not_pivot .!= false) || p_alive # while any alive variables

        # if pivot was previously frozen
        if !p_alive

            if num_frozen >= targ_frozen
                # println("recur: $(n), $(num_frozen), $(targ_frozen)")
                y = _gs_walk_recur(X[:,live_not_pivot], MC, z[live_not_pivot], lam, balanced, cov_sum)
                z[live_not_pivot] = y
                break
            end

            # select a new pivot by pivot rule
            p = sample_pivot(live_not_pivot, n - num_frozen)
            p_alive = true
            live_not_pivot[p] = false

            # downdate cholesky factorization by a_ratio * (a_p a_p') now that p has been decided
            lowrankdowndate!(MC, X[:,p])
            if balanced
                cov_sum -= X[:,p]
            end
        end

        # get the u vector (only defined on live no pivots)
        # this involves computing a vector a and also a vector b in the balanced case

        # Here is a description of the a, more clearly outlined in paper
        #   a(0) = X_k X_k' * z_p                                   O(d^2) using factorization
        #   a(1) = inv( lam/(1-lam) * I + X_k' * X_k ) * a(0)   O(d^2) using factorization
        #   a(2) = (1-lam)/(lam) [ a(1) - v_p]                  O(d)
        #   a(3) = X_k * a(2)                                       O(nd) matrix-vector multiplication

        a = (MC.L * (MC.U * X[:,p])) - (lam / (1-lam)) * X[:,p] # a(0)
        ldiv!(MC, a)                                                # a(1)

        mult_val = (1 - lam) / lam                              # a(2), in place
        for i=1:length(a)
            a[i] = mult_val * (a[i] - X[i,p])
        end
        a = (X' * a)[live_not_pivot]                                # a(3)

        if balanced

            # Here is a description of the b, more clearly outline in the paper
            #   b(0) = X_k' * 1                                         O(1) look-up (pre-computed)
            #   b(1) = inv( lam/(1-lam) * I + X_k' * X_k ) * b(0)   O(d^2) using factorization
            #   b(2) = X_k * b(1)                                       O(nd) matrix-vector multiplication
            #   b(3) = (b(2) - 1) / (2 * lam)                         O(n)

            b = copy(cov_sum)               # b(0)
            ldiv!(MC, b)                    # b(1)
            b = (X' * b)[live_not_pivot]    # b(2)

            div_val = 2*lam               # b(3), in place
            for i=1:length(b)
                b[i] = (b[i] - 1) / div_val
            end

            # compute scaling constant
            scale = - (1 + sum(a)) / (sum(b))

            # compute u
            u = a + (scale * b)
        else
            # u is a -- nothing to do, really
            u = a
        end

        # get the step size delta
        del_plus, del_minus = compute_step_sizes(z, u, live_not_pivot, p)
        prob_plus = del_minus / (del_plus + del_minus)
        del = (rand() < prob_plus) ? del_plus : -del_minus # randomly choose + or -

        # update z
        z[live_not_pivot] += del * u
        z[p] += del

        # update indices if they are frozen
        for i=1:n
            if live_not_pivot[i]

                # if frozen, update live not pivot array, cholesky factorization, and covariate sum
                if (abs(z[i]) >= 1. - tol)
                    live_not_pivot[i] = false
                    lowrankdowndate!(MC, X[:,i])
                    if balanced
                        cov_sum -= X[:,i]
                    end
                    num_frozen += 1
                end

            elseif p == i
                # a flag for whether pivot is alive
                p_alive = (abs(z[i]) < 1. - tol)
                if !p_alive
                    num_frozen += 1
                end
            end
        end

        # update iteration count
        iter += 1
    end

    return z
end

function sample_pivot(live_not_pivot, num_alive)
    """
    # sample_pivot
    # A fast impelmentation for uniformly sampling a pivot from the set of alive variables.
    # Note that this is only meant to be called when a pivot is frozen, so live_not_pivot == live
    #
    # Input
    #   live_not_pivot  n length bit array where a `true` entry means the variable is alive and not pivot
    #   num_alive       integer, the number of alive variables
    #
    # Output
    #   p               the randomly chosen pivot
    """
    ind = rand(1:num_alive)
    for p=1:length(live_not_pivot)
        if live_not_pivot[p]
           ind -= 1
            if ind == 0
                return p
            end
        end
    end
end

function compute_step_sizes(z, u, live_not_pivot, p)
    """
    # compute_step_sizes
    # Compute the positive and negative step sizes, without unecessary allocations & calculations.
    #
    # Input
    # 	z               n vector in [-1,1]
    #   u               m vector where m = # of non-pivot alive variables
    #   live_not_pivot  n length bit array where a `true` entry means the variable is alive and not pivot
    #   p               integer, pivot variable
    #
    # Output
    #   del_plus    the positive step size
    #   del_minus   the negative step size
    """

    # initialize + and - step sizes
    del_plus = Inf
    del_minus = Inf

    # set tolerance
    zero_tol = 10*eps()

    # go through all coordinates, finding best
    ind = 0
    for i=1:length(z)

        if live_not_pivot[i]

            ind += 1

            # skip the case where u is numerically zero
            if abs(u[ind]) <= zero_tol
                continue
            end

            # these are the step sizes delta_+ and delta_- that yield integrality
            dp = (sign(u[ind]) - z[i]) / u[ind]
            dm = (sign(u[ind]) + z[i]) / u[ind]

            # update step sizes to z[i] is always within +/- 1
            del_plus = (dp < del_plus) ? dp : del_plus
            del_minus = (dm < del_minus) ? dm : del_minus

        elseif p == i

                # these are step sizes delta_+ and delta_- that yield integrality
                dp = 1 - z[i]
                dm = 1 + z[i]

                # update step sizes to z[i] is always within +/- 1
                del_plus = (dp < del_plus) ? dp : del_plus
                del_minus = (dm < del_minus) ? dm : del_minus
        end
    end

    # return largest possible +/- step sizes
    return del_plus, del_minus
end

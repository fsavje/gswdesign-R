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

function compute_step_sizes(x, u, live_not_pivot, p)
    """
    # compute_step_sizes
    # Compute the positive and negative step sizes, without unecessary allocations & calculations.
    #
    # Input
    # 	x               n vector in [-1,1]
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
    for i=1:length(x)

        if live_not_pivot[i]

            ind += 1

            # skip the case where u is numerically zero
            if abs(u[ind]) <= zero_tol
                continue
            end

            # these are the step sizes delta_+ and delta_- that yield integrality
            dp = (sign(u[ind]) - x[i]) / u[ind]
            dm = (sign(u[ind]) + x[i]) / u[ind]

            # update step sizes to x[i] is always within +/- 1
            del_plus = (dp < del_plus) ? dp : del_plus
            del_minus = (dm < del_minus) ? dm : del_minus

        elseif p == i

                # these are step sizes delta_+ and delta_- that yield integrality
                dp = 1 - x[i]
                dm = 1 + x[i]

                # update step sizes to x[i] is always within +/- 1
                del_plus = (dp < del_plus) ? dp : del_plus
                del_minus = (dm < del_minus) ? dm : del_minus
        end
    end

    # return largest possible +/- step sizes
    return del_plus, del_minus
end

function sample_gs_walk(A; alpha=0.5, x = zeros(size(A,2)), verbose=false, balanced=false)
    """
    # sample_gs_walk
    # An fast implementation for sampling from the Gram-Schmidt Walk Design.
    # Maintains a cholesky factorization of (I + A * A^T ) for faster repeated linear system solves
    # and has a recursive component for more effective memory allocation.
    #
    # Input
    # 	A               an d by n matrix which has the vectors a_1, a_2 ... a_n as columns
    #   alpha           a real value in (0,1) specifying weight
    #   verbose         set to true to print out algorithm state, default is false
    #   balanced        set `true` to run the balanced GSW; otherwise, leave false
    #
    # Output
    #   x               the random +/- 1 vector, length n array
    """

    # get the dimensions
    d, n = size(A)

    # pre-processing: compute cholesky factorization of I + (1-a) A A^T
    M = (alpha / (1-alpha)) * I +  (A * A')
    MC = cholesky(M)

    # compute sum of covariances if necessary
    if balanced
        cov_sum = sum(A, dims=2)
    else
        cov_sum = nothing
    end

    # run the recursive version of the walk
    x = _gs_walk_recur(A, MC, x, alpha, verbose, balanced, cov_sum)

    # throw an error if |x| is outside of 1 +/- tol (this shouldn't happen)
    tol = 100*eps()
    @assert maximum(abs.((abs.(x) .- 1))) <= tol

    # clean x to be exactly +/- 1
    x = sign.(x)
    return x
end

function _gs_walk_recur(A, MC, x, alpha, verbose, balanced, cov_sum)
    """
    # _gs_walk_recur
    # Carries out the iterative procedure of the Gram--Schmidt walk.
    # This function recusively cals itself after sufficiently many variables have been frozen to achieve
    # better memory allocation. The cholesky factorization of (I + A * A^T ) is also maintained.
    #
    # Input
    # 	A               an d by n matrix which has the vectors a_1, a_2 ... a_n as columns
    #   alpha           a real value in (0,1) specifying weight
    #   verbose         set to true to print out algorithm state, default is false
    #   balanced        set `true` to run the balanced GSW; otherwise, leave false
    #
    # Output
    #   x               the random +/- 1 vector, length n array
    """

    # get the dimensions, set tolerance
    d, n = size(A)
    tol = 100*eps()

    # initialize alive variablesa and pivot index
    live_not_pivot = trues(n) # bit array, space efficient
    p_alive = true

    # select pivot, update cholesky and covariate sum
    p = rand(1:n)
    live_not_pivot[p] = false
    lowrankdowndate!(MC, A[:,p])
    if balanced
        cov_sum -= A[:,p]
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
                y = _gs_walk_recur(A[:,live_not_pivot], MC, x[live_not_pivot], alpha, verbose, balanced, cov_sum)
                x[live_not_pivot] = y
                break
            end

            # select a new pivot by pivot rule
            p = sample_pivot(live_not_pivot, n - num_frozen)
            p_alive = true
            live_not_pivot[p] = false

            # downdate cholesky factorization by a_ratio * (a_p a_p') now that p has been decided
            lowrankdowndate!(MC, A[:,p])
            if balanced
                cov_sum -= A[:,p]
            end
        end

        # get the u vector (only defined on live no pivots)
        # this involves computing a vector a and also a vector b in the balanced case

        # Here is a description of the a, more clearly outlined in paper
        #   a(0) = X_k X_k' * x_p                                   O(d^2) using factorization
        #   a(1) = inv( alpha/(1-alpha) * I + X_k' * X_k ) * a(0)   O(d^2) using factorization
        #   a(2) = (1-alpha)/(alpha) [ a(1) - v_p]                  O(d)
        #   a(3) = X_k * a(2)                                       O(nd) matrix-vector multiplication

        a = (MC.L * (MC.U * A[:,p])) - (alpha / (1-alpha)) * A[:,p] # a(0)
        ldiv!(MC, a)                                                # a(1)

        mult_val = (1 - alpha) / alpha                              # a(2), in place
        for i=1:length(a)
            a[i] = mult_val * (a[i] - A[i,p])
        end
        a = (A' * a)[live_not_pivot]                                # a(3)

        if balanced

            # Here is a description of the b, more clearly outline in the paper
            #   b(0) = X_k' * 1                                         O(1) look-up (pre-computed)
            #   b(1) = inv( alpha/(1-alpha) * I + X_k' * X_k ) * b(0)   O(d^2) using factorization
            #   b(2) = X_k * b(1)                                       O(nd) matrix-vector multiplication
            #   b(3) = (b(2) - 1) / (2 * alpha)                         O(n)

            b = copy(cov_sum)               # b(0)
            ldiv!(MC, b)                    # b(1)
            b = (A' * b)[live_not_pivot]    # b(2)

            div_val = 2*alpha               # b(3), in place
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
        del_plus, del_minus = compute_step_sizes(x, u, live_not_pivot, p)
        prob_plus = del_minus / (del_plus + del_minus)
        del = (rand() < prob_plus) ? del_plus : -del_minus # randomly choose + or -

        # print variables
        if verbose
            println("\n\nIteration ", iter)
            println("\tpivot is ", p)
            println("\tAlive vars (not pivot) \t", live_not_pivot)
            println("\tx is\t", x)
            println("\tu is \t", u)
            println("\tdelta_+ is \t", del_plus)
            println("\tdelta_- is \t", del_minus)
            println("\tProb plus: \t", prob_plus)
        end

        # update x
        x[live_not_pivot] += del * u
        x[p] += del

        # update indices if they are frozen
        for i=1:n
            if live_not_pivot[i]

                # if frozen, update live not pivot array, cholesky factorization, and covariate sum
                if (abs(x[i]) >= 1. - tol)
                    live_not_pivot[i] = false
                    lowrankdowndate!(MC, A[:,i])
                    if balanced
                        cov_sum -= A[:,i]
                    end
                    num_frozen += 1
                end

            elseif p == i
                # a flag for whether pivot is alive
                p_alive = (abs(x[i]) < 1. - tol)
                if !p_alive
                    num_frozen += 1
                end
            end
        end

        # update iteration count
        iter += 1
    end

    return x
end

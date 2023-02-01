module Problems.Problem28 where



problem28 :: IO () 
problem28 = do 
    {- The top right vertex of the square can be found using (2k+1)**2.
        The sum of diagonal numbers can be easily obtained from symmetry. 
        With the formula of the sum of sequences, you can find the answer more easily. 
          -}

    let l = 500
    print $ 2 * sum [8*k*k + 2*k + 1 | k <- [0..l]]

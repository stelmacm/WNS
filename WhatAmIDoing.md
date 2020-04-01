## What am I doing with my life (and this project)

### Week of March 22nd

- Reading about RandomFields package
- Played around with several examples from Wash. State Uni
- These examples were mostly Gaussian Spatial Autocorrelation simulations
- Cool Examples but still need to learn the more spatially intensive versions
- Read https://www.pnas.org/content/suppl/2020/03/11/1914794117.DCSupplemental 
- I think this is a cool interesting paper which tells us about the characteristics of the disease and should tell us some info
  about what to do with SIR or SIS or SIRS decisions and assumptions
- Trying exaples of Seedy (epi package) for networking disease
- I keep finding myself convinced that the answer for all my questions is in https://www.repidemicsconsortium.org/projects/ but I just
  don't know in which
- Uploading Proof for Ben
- Played around with spreadr but its probably not it but just interested me 
- Ran through Ben's examples in spatial and spatial_lab (Summary: Stuff I probably should have looked at first that summarizes everything
  I have just done up there ^)
- Forgot to push this yesterday. oops. Need to get better with pushing
- https://rpubs.com/nabilabd/118172 <- article about Kriging with sp 
- I would like to learn more about SPDF because I feel like my knowledge there is very lacking
- https://stats.stackexchange.com/questions/248113/are-50-confidence-intervals-more-robustly-estimated-than-95-confidence-interva
- Andrew Gelman's paper

### Week of March 28th

- Moved out of student house (yay home and human interaction)
- Forecasting proposal
- Kernel smoothing assignment
- Read EpiModel cran
- Ran through examples of Epimodel to see what it looks like
- Interested in Epimodel but I think it is ultimately better to do this from scratch
- CSE courses essentially all have the theme of don't do anything yourself use packages bc they're better
  and the course just teaches you a bunch of packages
- I think for alot of projects writing your own functions is better because you will know exactly what is happening
- Created dummy simulation in randon nonsense
- Dummy simulation makes sense and works properly for 6x6 matrix however I think updating the weight matrix for every year
  iteration needs to be done better
- This did show me that I had alot of things created incorrectly before. Ie. relevant.records needs to be cleaned better
- Centroids for weight matrix were done correctly the first time (So correct in this repo but not mine and Adrians)
- Error in centroids shows up everynow and then but then is fixed when I restart R??
- Should I be saving sim results from each year or is it sufficient to simply return the final value of simulation (I know the answer but I feel like I'm asking if there is a better way of saving them other than in a new dataframe that has per year)
- Big problem is logistics of how to approach locations that die out or are created inbetween the 8 year period
- Tried to just ignore those and use simply those that last the whole time but thats less than 100
- Ultimately just ran a simulation and kept the weight matrix and kept the infected at 0 until it was created and then used the value it is assigned when created. When locations are closed they just continue carrying the value they have and obviously changes if the simulation changes it. Are these bold assumptions? I know the golden rule is as long as you can justify it to yourself but I wonder if there is a better approach
- My as.numeric is converting the as.factor to 2 and 1 instead of 1 and 0. It would be pretty bad coding to code 2 as 1 and 1 as 0...
Need to find a way to ensure as.factor converts nicely.
- Ultimately challenge lies in logistics of how to do a more formal simulation
- Did not get to fitting baseline hazard model
- I think I want to segment the individual years from the dataframe and basically run through each year. (currently just have 1 big simulation happening in attempt to start small and move up)
- Not many things flow too nicely with drake so I might need to reorganize things in a nicer way so Ben can just run them quicker and easier instead of sifting through unorganized code.
- Currently everything is mixed in here and ther in spatial-weight-matrix.R, geocache-weights.R, randomnonsense and randomscripts on my laptop (try and out it all in one new one)
- So I just did everything wrong but now I understand everything??? There's no positive here. I just have to redo it. 
- Initial vector isn't correct. The vector is only for each given location when they appear for the first time. If I do only ones that appear only in 2008 (ie initial it makes it 31 counties that are recorded). I guess the problem might be the sparsity of the weight matrix. I get now that ivec should be updating itself but I cant see how the weight matrix should be changing from year to year if infection state matrix is.
 

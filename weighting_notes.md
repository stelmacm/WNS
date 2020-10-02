For now we'll focus on a particular year $t$.

For county $i$, infection status (0/1) in year $t$ is the response (we're only interested in county $i$ if it was *uninfected* in year $t-1$, otherwise we assume it remains infected

The effect of an *infected* county $j$ in year $t-1$ on the log-hazard of infection is $W_{ij}$.  So the total log-hazard of infection of county $i$ in year $t$ is $W_{i} I_{t-1}$, where $W_i$ is the $i^{\textrm{th}}$ row of the weight matrix and $I_{t-1}$ is the incidence vector in year $t-1$ (equivalently we have $\sum_{j \in I_{t-1}} W_{ij}$, the sum of the weights corresponding to the infected counties in the previous year (because uninfected counties drop out of the sum).

So (assuming $W$ stays constant over time) for each year we construct $W I_{t-1}$ (which is a vector); this is the *weighted incidence*.  If $W_{ij}=1$ for all $i$, $j$ then this would be the number of infected counties in the previous year.

```
for (y in 1:numyears) {
	foi <- W %*% incidence
	...
}
```

do this: with a `for` loop (at the end you want to assemble one big data frame that has all years, for every year it includes all counties that were uninfected in the previous year, along with the foi for that county for that year)

W: e.g. 100x100

year 1: 98 uninfected counties, 2 infected counties
W %*% I(1) (-> vector of length 100), 
   select the elements corresponding to the two infected counties

Suppose Imat is a matrix (n rows x t columns) of the incidence in county i, year j

W %*% Imat -> (n x n) %*% (n x t) -> (n x t) of the foi on the county i in year j
melt that and join it with the list of counties that are uninfected

before we were calculating glm(incidence ~ offset(log(prev_infection)) ...)

that prev_infection (== sum of the incidence in the previous year)
now *becomes* the foi we've calculated (foi == force of infection)

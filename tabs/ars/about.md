# <h1 style = 'color:black'><label class='control-label'>About the app</label></h1>

This app was presented (with some modifications) in a Bayesian Inference discipline on June-2015. The idea was to show how the ARS method works and visualize this in each step. The Shiny app was the best way to do this.

# <h1 style = 'color:black'><label class='control-label'>ARS method</label></h1>

Sometimes we work with unknown distributions and evaluate the distribution is computationally expensive. The ARS method arises as a way to solve this problem and its biggest advantage is the small number of evalutions of the h(x), where h(x) is the logarithm of the distribution of interest (normalized or not).
The idea of the method is to use indirect simulation to sample of a log-concave and univariate distribution. To achieve this goal, the method calculates the function h(x) which enables the evaluation of two functions: the upper and lower hull. To construct them it is necessary to define the number of initial points and the boundaries to sample of this distributions. For example, to sample from a gamma distribution the boundaries can be 0 and infinity.

The upper hull is based on the tangent line and the lower hull is based on the line between two points. The idea is to approximate the upper and lower hull of h(x), for this some iterations are necessary. For each iteration one observation is sampled for upper hull and this observation is accepted or not depending on two tests. The first test is called squeezing test and it is the ratio of upper and lower hull aplied in the observation point. If this distance is small, the approximation is good. If the observation is rejected in this step so the rejection test is done and it corresponds to ratio of upper hull and objective function. If the distance is small the approximation is good.

If the observation is rejected in squeezing test so this point is used to refine the upper and lower hull and this observation is not accepted. If the observation is accepted in the rejection test so the upper and lower hull are refined and this observation is accepted. If the observation is rejected in rejection test so the upper and lower hull are refined and the observation is rejected.

The number of evalutions of h(x) decreases in each step because the envelope approximation becomes good enough.

By using this app it is possible to see how the method works and how the method envelopes the distribution and the log-distribution. 

# <h1 style = 'color:black'><label class='control-label'>Short tutorial</label></h1>

Here are simple steps to use this app:

* Select one distribution of the list. They are log-concave distributions.
* Set the parameters of this distribution.
* Select the boundaries of the distribution. If you change the boundaries, truncated distributions are simulated.
* Select the number of points to sample with ARS.
* Select the number of initial points (to build the upper and lower hull).
* Push the RUN! button.

After pressing RUN, four graphs are going to appear. 

The left graph presents the logarithm of the distribution and we can see the upper and lower hull based on the initial points (setted previously). If you have set a small number of initial points these functions will be clearly different.

The graph in the middle is the histogram (with the density to compare) of the generated observations. At the first time you can see just the density because we are in the first iteration.

The right graph is the distribution and the upper and lower hull in the original scale called envelope function and squeezing function respectively. It corresponds to the exponential of the function in the left graph.

Now you can set the iteration number with the new button on top of the panel to see step by step the method generating the sample. The evolution of acceptance rate appears in the new graph, below the others.

Go ahead!

# <h1 style = 'color:black'><label class='control-label'>Source Code</label></h1>
Source code: [GitHub](https://github.com/DouglasMesquita/shinyverse/tree/master/tabs/ars)

# <h1 style = 'color:black'><label class='control-label'>Acknowledgement</label></h1>

I acknowledge to [Larissa Sayuri](http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K8243295P6) and [Luís Gustavo](http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4323569E0) for the considerations and help in this work.

# <h1 style = 'color:black'><label class='control-label'>References</label></h1>

To read more about the ARS method try:

1. [Adaptive Rejection Sampling for Gibbs Sampling](http://www.jstor.org/stable/2347565?seq=1#page_scan_tab_contents)
2. [Monte Carlo Statistical Methods](http://www.springer.com/us/book/9780387212395)
3. [Adaptive Rejection Sampling](https://stat.duke.edu/~cnk/Links/slides.pdf)

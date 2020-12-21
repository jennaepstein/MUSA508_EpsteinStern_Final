# Predicting wildfire risk and developing an app to distribute emergency supply kits

## About the project

We want to work with public sector agencies at the county level to strategically distribute emergency supply kits to the right individuals.

To facilitate this engagement, we developed an algorithm and an application. The algorithm has two parts that feed into a composite score: fire probability and vulnerability. For fire probability, we built a model to predict for fire occurrences using environmental risk factors and other spatial features. The predicted probabilities were then incorporated with the Social Vulnerability Index provided by the Center for Disease Control and Prevention.

This final algorithm outputs a prioritized list of census tracts to target. Census tracts are prioritized based in equal parts on the probability of a fire occurring and the Social Vulnerability Index score for that tract. In practice, this will help ensure that kits make it to both those most in-need and those that are most vulnerable to emergencies like wildfires. The algorithm is accessible via an interactive application that allows users to tweak inputs such as budget and cost of the kits. In this way, users can visualize the exact areas and total number of households served. 

--

# Explore the project

[Video pitch](https://www.youtube.com/watch?v=LJHMXxgcA64)

[Deep dive of the algorithm](https://jeffstern.github.io/epsteinstern-analysis.html)

[Interactive application](https://jeffstern.shinyapps.io/Emergency-Kit-Distribution/)

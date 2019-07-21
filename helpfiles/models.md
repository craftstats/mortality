---
output: 
  html_document: 
    keep_md: yes
    mathjax: default
---

## Class of models
Los modelos disponibles son los que se encuentran en el paquete [stMoMo](https://cran.r-project.org/web/packages/StMoMo/vignettes/StMoMoVignette.pdf).     

<hr/>

$$\begin{array}{lcl}
\text{LC: Lee-Carter   } &  & η_{xt} = α_x +{β^{(1)}_x}{κ^{(1)}_t}
\end{array}$$
$$\begin{array}{lcl}
\text{CBD: Cairns-Blake-Dowd} &  & η_{xt} = κ_t^{(1)} + (x-\bar{x})κ_t^{(2)}
\end{array}$$
$$\begin{array}{lcl}
\text{APC: Age-Period-Cohort} &  & η_{xt} = α_x + κ_t + γ_{t-x}
\end{array}$$
$$\begin{array}{lcl}
\text{RH ("NP") : Renshaw and Haberman} &  &  η_{xt} = α_x + β^{(1)}_xκ_t + β^{(0)} γ_{t-x}
\end{array}$$
$$\begin{array}{lcl}
\text{RH ("1") : Renshaw and Haberman} &  &  η_{xt} = α_x + β^{(1)}_xκ_t + γ_{t-x}
\end{array}$$

$$\begin{array}{lcl}
\text{M6: } &  & η_{xt} = κ_t^{(1)} + (x-\bar{x})κ_t^{(2)} + γ_{t-x}
\end{array}$$
$$\begin{array}{lcl}
\text{M7: } &  & η_{xt} =κ_t^{(1)} + (x-\bar{x})κ_t^{(2)} + ((x-\bar{x})^2 - \hat{σ}^2_x)κ_t^{(3)} + γ_{t-x}
\end{array}$$
$$\begin{array}{lcl}
\text{M8: } &  &  η_{xt} = κ_t^{(1)} + (x-\bar{x})κ_t^{(2)} + (x_c-x)γ_{t-x}
\end{array}$$
$$\begin{array}{lcl}
\text{PLAT: } &  & η_{xt} =  α_x  + κ_t^{(1)} + (x-\bar{x})κ_t^{(2)} + γ_{t-x}
\end{array}$$
one_metric_example_data <- tar_read(activity_by_type_clusters_stg6) |>
  as_tibble()|>
  select(cluster2,metric1_total,starts_with("metric29"))|>
  rename(denominator=metric1_total,numerator=metric29_total)


# Function to compute index of disparity
compute_id <- function(one_metric_example_data) {
  # Compute index of disparity
  data <- one_metric_example_data|>
    mutate(upper_ci=(numerator+(sqrt(numerator)*1.96))*(1/denominator),
           lower_ci=(numerator-(sqrt(numerator)*1.96))*(1/denominator),
           rate=numerator/denominator,
           se=(rate-upper_ci)/1.96,
           sd=(rate-mean(numerator)),
           global_rate=sum(numerator)/sum(denominator),
           diff = rate-global_rate,
           abs_diff=abs(diff*denominator))
  iod_abs <- sum(data$abs_diff)/2
  iod_rel <- sum(data$abs_diff)/(2*sum(data$numerator))
  return(iod_rel)
}


data <- one_metric_example_data
i<-1
n<-as.numeric(nrow(data))
for (i in 1:n){
  num=data$numerator[i]
  denom=data$denominator[i]
  rate=num/denom
  upper_ci=(num+(sqrt(num)*1.96))*(1/denom)
  se=(upper_ci-rate)/1.96
  rates <- rnorm(n=1000,rate,se)
  if(i==1)
   { rate_clusters <- as.data.frame(rates) }
  else 
  { rate_clusters <- rate_clusters|> bind_cols(as.data.frame(rates))}
  i=i+1
}

rate_clusters <- rate_clusters |>
  rename(rates1=`rates...1`,
         rates2=`rates...2`,
         rates3=`rates...3`,
         rates4=`rates...4`,
         rates5=`rates...5`
        )|>
  mutate(num1=rates1*data$denominator[1],
         num2=rates2*data$denominator[2],
         num3=rates3*data$denominator[3],
         num4=rates4*data$denominator[4],
         num5=rates5*data$denominator[5],
         denom1=data$denominator[1],
         denom2=data$denominator[2],
         denom3=data$denominator[3],
         denom4=data$denominator[4],
         denom5=data$denominator[5]
  )


rate_clusters <- rate_clusters |>
mutate(upper_ci1=(num1+(sqrt(num1)*1.96))*(1/denom1),
       lower_ci1=(num1-(sqrt(num1)*1.96))*(1/denom1),
       rate1=num1/denom1,
       se1=(rate1-upper_ci1)/1.96,
       upper_ci2=(num2+(sqrt(num2)*1.96))*(1/denom2),
       lower_ci2=(num2-(sqrt(num2)*1.96))*(1/denom2),
       rate2=num2/denom2,
       se2=(rate2-upper_ci2)/1.96,
       upper_ci3=(num3+(sqrt(num3)*1.96))*(1/denom3),
       lower_ci3=(num3-(sqrt(num3)*1.96))*(1/denom3),
       rate3=num3/denom3,
       se3=(rate3-upper_ci3)/1.96,
       upper_ci4=(num4+(sqrt(num4)*1.96))*(1/denom4),
       lower_ci4=(num4-(sqrt(num4)*1.96))*(1/denom4),
       rate4=num4/denom4,
       se4=(rate4-upper_ci4)/1.96,
       upper_ci5=(num5+(sqrt(num5)*1.96))*(1/denom5),
       lower_ci5=(num5-(sqrt(num5)*1.96))*(1/denom5),
       rate5=num5/denom5,
       se5=(rate5-upper_ci5)/1.96,
       global_rate=(num1+num2+num3+num4+num5)/(denom1+denom2+denom3+denom4+denom5),
       diff1 = rate1-global_rate,
       abs_diff1=abs(diff1*denom1),
       diff2 = rate2-global_rate,
       abs_diff2=abs(diff2*denom2),
       diff3 = rate3-global_rate,
       abs_diff3=abs(diff3*denom3),
       diff4 = rate4-global_rate,
       abs_diff4=abs(diff4*denom4),
       diff5 = rate5-global_rate,
       abs_diff5=abs(diff5*denom5),
       iod_abs=(abs_diff1+abs_diff2+abs_diff3+abs_diff4+abs_diff5)/2,
       iod_rel=(abs_diff1+abs_diff2+abs_diff3+abs_diff4+abs_diff5)/(2*(num1+num2+num3+num4+num5)))


compute_id(one_metric_example_data)
quantile(rate_clusters$iod_rel, c(.05, .95)) 


require(dplyr)

v1 = c(rep('one',5),rep('two',5))
v2 = c(rep(c('one','two'),5))
x1 = rpois(10,10)
x2 = rpois(10,25)
x3 = rpois(10,100)

df = data.frame(v1,v2,x1,x2,x3)
rm(v1,v2,x1,x2,x3)

df%>%group_by(v1)%>%
  summarise(mean_var = mean(x3))

#setting varaibles
my_var = v1
my_string = 'v1'

# A failure
df%>%group_by(my_string)%>%
  summarise(mean_var = mean(x3))

#enter the quosure
my_var = quo(v1)
my_var

#if you look at the errors you can see that group by is trying to quote the imput, we have to tell it 
#not to quote the input that we have already taken care of it
df%>%
  group_by(my_var)%>%
  #group_by(!!my_var)%>%
  summarise(mean_var = mean(x3))

#lets try a function
my_mean = function(data, group_var, mean_var){
  data%>%group_by(!!group_var)%>%
    summarise(mean_out = mean(!!mean_var))
}

#trying the function
my_mean(df,quo(v1),quo(x3))

#try to make it look nicer
my_mean = function(data, group_var, mean_var){
  group_var = quo(group_var)
  print(group_var)
  mean_var = quo(mean_var)
  print(mean_var)
  
  data%>%group_by(!!group_var)%>%
    summarise(mean_out = mean(!!mean_var))
}

#watch it fail
my_mean(df,v1,x3)
my_var

#who has two thumbs and read the documentation

my_mean = function(data, group_var, mean_var){
  group_var = enquo(group_var)
  print(group_var)
  mean_var = enquo(mean_var)
  print(mean_var)
  
  data%>%group_by(!!group_var)%>%
    summarise(mean_out = mean(!!mean_var))
}
my_mean(df,v1,x3)


#adding a dynamic name varaible
my_mean = function(data, group_var, mean_var){
  group_var = enquo(group_var)4444444444444444444444444444444444444444444444444444444
  mean_var = enquo(mean_var)
  mean_name = paste0("mean_", quo_name(mean_var))
  print(mean_name)
  
  data%>%group_by(!!group_var)%>%
    summarise(!!mean_name := mean(!!mean_var))
}
my_mean(df,v1,x3)

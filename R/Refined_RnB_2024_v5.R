
# Function to assign groups Red and Black
red_and_black <- function(k) {
  set.seed(k)
  N <- nrow(class_roll)
  
  # Assign groups
  class_roll$group <- sample(1:N) %% 2 %>%
    factor(levels = c(0, 1), labels = c("Red", "Black"))
  
  # Extract year from student ID
  class_roll$id_2 <- class_roll$id %>%
    substr(1, 4) %>%
    ifelse(. <= 2018, "2018", .)
  
  # Chi-squared test for year distribution
  X1 <- class_roll %$%
    table(.$group, .$id_2) %>%
    chisq.test(simulate.p.value = FALSE) %>%
    `[[`(1) %>%
    unname
  
  # ISP Email Provider Analysis
  isp <- class_roll$email %>%
    strsplit("@", fixed = TRUE) %>%
    sapply("[", 2) %>%
    strsplit("[.]", fixed = FALSE) %>%
    sapply("[", 1)
    
  X4 <- isp %>%
    ifelse(. %in% c("naver", "gmail"), ., "기타서비스") %>%
    factor(levels = c("naver", "gmail", "기타서비스"),
           labels = c("네이버", "구글", "기타서비스")) %>%
    table(class_roll$group, .) %>%
    chisq.test(simulate.p.value = FALSE) %>%
    `[[`(1) %>%
    unname

  # Return results as a list
  list(X1 = X1, X4 = X4)
}

# Example usage
result <- red_and_black(123)
print(result)

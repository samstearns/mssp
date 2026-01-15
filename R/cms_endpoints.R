#' Generates dataframe with links to CMS data for each year
#' @return Data frame with URL to get CMs data
#' @examples
#' get_cms_endpoints()
#' @export
get_cms_endpoints <- function() {
  cms_endpoints <- data.frame(year=c(2013), url=c("https://data.cms.gov/data-api/v1/dataset/bc90f498-76f4-4e75-8225-8aae30336059/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2014, "https://data.cms.gov/data-api/v1/dataset/0ef9b1e2-e23b-4a01-921c-1ac7290c814b/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2015, "https://data.cms.gov/data-api/v1/dataset/156c00e2-ab42-4923-b54f-09c031f5f28d/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2016, "https://data.cms.gov/data-api/v1/dataset/a290fdd3-976a-4fc9-9139-a98193b3af82/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2017, "https://data.cms.gov/data-api/v1/dataset/3b306450-1836-417b-b779-7d70fd2fc734/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2018, "https://data.cms.gov/data-api/v1/dataset/80c86127-8839-4f35-b87b-aa37664afd19/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2019, "https://data.cms.gov/data-api/v1/dataset/9c3a4c69-7d00-4307-9b6f-a080dc90417e/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2020, "https://data.cms.gov/data-api/v1/dataset/8f073013-9db0-4b12-9a34-5802bdabbdfe/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2021, "https://data.cms.gov/data-api/v1/dataset/a5d74ce2-ba38-47be-8523-146e4ad41832/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2022, "https://data.cms.gov/data-api/v1/dataset/a5d74ce2-ba38-47be-8523-146e4ad41832/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2023, "https://data.cms.gov/data-api/v1/dataset/7082a8f1-6d51-4723-853d-086bf254f5fb/data"));
  cms_endpoints <- rbind(cms_endpoints, list(2024, "https://data.cms.gov/data-api/v1/dataset/73b2ce14-351d-40ac-90ba-ec9e1f5ba80c/data"));
  return(cms_endpoints);
}

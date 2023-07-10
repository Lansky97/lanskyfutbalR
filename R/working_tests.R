#TESTS
library(worldfootballR)

matches = as_tibble(fb_match_results(country = "ENG", gender = "M", season_end_year = "2023"))

test_ratings = matches %>%
                matches_to_team_ratings()

test = matches %>%
        sample_n(10)

#MAtches to fictures test

test_fixtures = test %>%
  matches_to_fixtures()

test_gw_fixtures = test_fixtures %>%
                    filter(GW == 5)



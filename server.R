models.UBCF = readRDS('UBCF.rds')
models.IBCF = readRDS('IBCF.rds')

OPTIMAL_N = 10
library(dplyr)

ratings = read.csv(
  'ratings.dat', 
  sep = ':',
  colClasses = c('integer', 'NULL'), 
  header = FALSE
)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
ratings_matrix = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(ratings_matrix) = levels(tmp$i)
colnames(ratings_matrix) = levels(tmp$j)
ratings_matrix = new('realRatingMatrix', data = ratings_matrix)


# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(
  movies$MovieID, 
  function(x) paste0(small_image_url, x, '.jpg?raw=true')
)

# Get unique movie genres for dropdown
unique_genres = c()
for (unsplit_genres in movies$Genres) {
  split_genres = strsplit(unsplit_genres[1], "|", fixed = TRUE)
  # Not sure why split_genres returns a list of list instead of a single list
  for (genresArr in split_genres) {
    for (eachGenre in genresArr) {
      if (!(eachGenre %in% unique_genres)) {
        unique_genres = append(unique_genres, eachGenre)
      } 
    }
  }
}

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# System 1
get_top_N_movies_data_by_genre = function(N, genre, ratings, movies, small_image_url, min_num_ratings=1000) {
  ranked_movies = ratings %>% 
    group_by(MovieID) %>% # group ratings by movie id
    summarize(
        num_ratings = n(), 
        ave_ratings = round(mean(Rating), dig=4),
        ave_ratings_timestamp = round(mean(Timestamp), dig=4),
    ) %>%
    inner_join(movies, by = 'MovieID') %>% # join with other movie data
    replace(is.na(.), 0) %>% 
    filter(num_ratings > min_num_ratings, grepl(genre, Genres)) %>% #filter movies with few ratings and filter by genre
    mutate(ave_ratings_rank = dense_rank(ave_ratings)) %>% 
    mutate(num_ratings_rank = dense_rank(num_ratings)) %>% 
    mutate(ave_ratings_timestamp_rank = dense_rank(ave_ratings_timestamp)) %>%
    arrange(desc(ave_ratings), desc(num_ratings), desc(ave_ratings_timestamp)) %>%
    mutate(final_rank = as.double(ave_ratings_rank + ave_ratings_timestamp_rank + num_ratings_rank)) %>%
    top_n(N, (final_rank)) %>%
    select('MovieID', 'Title', 'final_rank', 'ave_ratings_rank', 'ave_ratings_timestamp_rank', 'num_ratings_rank') %>%
    arrange(desc((final_rank))) %>%
  return(ranked_movies)
}


# # System 2
# predict_CF = function(active_user) {
#   tmp = matrix(data=NA, 1, length(movieIDs))
#   colnames(tmp) = movieIDs
#   tmp[1, active_user$MovieID] = active_user$Rating
#   r.pred = predict(r.model, as(tmp, "realRatingMatrix"), OPTIMAL_N)
#   return(as(r.pred, "list"))
# }

# # System 2
predict_CF = function(active_user) {
  active_matrix = matrix(data=NA, 1, ncol(ratings_matrix))
  colnames(active_matrix) = c(seq.int(1,ncol(ratings_matrix)))
  active_matrix[1, active_user$MovieID] = active_user$Rating
  pred = predict(models.IBCF, as(active_matrix, "realRatingMatrix"), OPTIMAL_N)
  return(as(pred, "list"))
}


# App
shinyServer(function(input, output, session) {
  # show the books to be rated
  
  output$ratings_book_grid <- renderUI({
    num_rows <- 20
    num_movies <- 5 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  outputOptions(output, "ratings_book_grid", suspendWhenHidden = FALSE)  
  
  # show genre dropdown
  output$genres_dropdown <- renderUI({
    selectInput("genreDropdown", "Genre:", as.list(unique_genres))
  })
  
  #Hide ratings container
  transition_to_loading_state <- function() {
    useShinyjs()
    jsCode <- "document.querySelector('[data-widget=collapse]').click();"
    runjs(jsCode)
  }
  
  df_genre <- eventReactive(input$btnGenre, {
    withBusyIndicatorServer("btnGenre", {
      #transition_to_loading_state()
      value_list = reactiveValuesToList(input)
      selected_genre = value_list$genreDropdown
      top_genre_movies = get_top_N_movies_data_by_genre(OPTIMAL_N, selected_genre, ratings, movies, small_image_url, 1000)
      user_results = (1:10)/10
      recom_genre_results <- data.table(Rank = 1:10, 
                                  MovieID = top_genre_movies$MovieID, 
                                  Title = top_genre_movies$Title, 
                                  Predicted_rating =  user_results)
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btnRating, {
    withBusyIndicatorServer("btnRating", { # showing the busy indicator
      #transition_to_loading_state()
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      user_results = (1:10)/10
      user_predicted_ids = predict_CF(user_ratings) # 
      user_predicted_ids = lapply(user_predicted_ids, function(x) substring(x,2))
      user_predicted_ids = as.numeric(unlist(user_predicted_ids))
    }) # still busy
    
  }) # clicked on button
  
  output$results_by_genre <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result = df_genre()
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        movie_idx = i * j
        movie_id = recom_result$MovieID[movie_idx]
        movie_title = recom_result$Title[movie_idx]
        rec_movie = movies[movies$MovieID == movie_id,]
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            div(style = "text-align:center", 
                a(img(src = rec_movie$image_url, height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movie_title)
            )
            
        )        
      }))) # columns
    }) # rows
  })
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_movie_ids <- df()
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        movie_idx = i * j
        movie_id = recom_movie_ids[movie_idx]
        rec_movie = movies[movies$MovieID == movie_id,]
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            div(style = "text-align:center", 
                a(img(src = rec_movie$image_url, height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(rec_movie$Title)
            )
            
        )         
      }))) # columns
    }) # rows
  }) # renderUI function
  
}) # server function
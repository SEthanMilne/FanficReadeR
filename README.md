<img align="right" src="https://user-images.githubusercontent.com/83033161/119401227-37682880-bca9-11eb-88d2-6b82f74847a8.png"> 

# FanficReadeR
**A webscraper for gathering public data on AO3**

Fanfiction is an interesting consumer area. You have a set of people who love a work so much that they dedicate countless hours of their time to creating their own stories inside the world of the story. There is little material gain for writing fanfic; authors are paid very little for their time (if at all), and writing a fanfic can sometimes feel like screaming one's thoughts into the void.

Yet fanfiction has also been the engine behind a number of pop culture mega-hits. Fifty Shades of Gray? [Started with a Twilight Fanfic](https://www.forbes.com/sites/hayleycuccinello/2017/02/10/fifty-shades-of-green-how-fanfiction-went-from-dirty-little-secret-to-money-machine/?sh=1886f89d264c). City of Bones (the book series) & Shadowhunters (the Netflix series)? [Started with a Harry Potter Fanfic](https://theweek.com/articles/460833/girls-film-confounding-problems-fan-fiction). More original content that is posted on fanfiction sites like Wattpad have [also led to Netflix TV series](https://techcrunch.com/2018/06/14/netflixs-latest-hit-the-kissing-booth-is-a-wattpad-success-story/).

It's not an exaggeration to say that media juggernauts like the Harry Potter books by J.K. Rowling owe a lot to the content their fans have generated; The over 800,000 HP Fanfics on one website, AO3, have certainly contributed to the maintenance of a passionate fanbase. I've made what I hope is a useful
tool for researchers and enthusiasts to easily get data about fanfiction communities.

***Note: at present, this package only scrapes data from AO3. This project was first targeted towards scraping from Fanfiction.net, but their website blocks all attempts at webscraping - perhaps due to their promised API coming soon. If and when that happens, I'll be making a companion package for collecting data from Fanfiction.net***

## The Package
`FanficReadeR` scrapes data from [ArchiveOfOurOwn](http://archiveofourown.org) (AO3), one of the world's leading fanfiction websites, with more than 3.7 million registered users and 7.6 million works listed on the platform. This package gathers data about two broad categories: **Users** and **Works**

### Works
Works are simply stories writting by users. They come in a variety of formats:
* One-Shots - short stories that typically have only 1 chapter
* Epics - the current [longest-fanfic](https://archiveofourown.org/works/7899862) is over 5 million words
* Joke Fics - the current [most-liked fic on AO3](https://archiveofourown.org/works/2080878) is one that simply repeats "I am Groot" over and over again.

Works can also be organized into **series**, or collections of individual fanfics to tell some broader story or to explore similar themes. Some users, for example, may write a series of Harry Potter fanfictions that follow a similar structure as the 7-book original series.

There are also a series of metrics by which to evaluate works:
* Length: defined as either chapter-count or word-count
* Kudos: the AO3 equivalent of "likes"
* Comments: responses to the fic generated by readers (with timestamps)
* Bookmarks: the number of times a fic has been "saved" by a reader to display on their profile
* Pairings: denotes the romantic/sexual pairings of characters in the story (F/F, F/M, M/M, Multi, etc)
* Fandoms: lists all the fandoms a work is associated with. This is important because many fics are "crossovers", using intellectual property from multiple different story universes (e.g.: a Harry Potter/Twilight crossover might have Bella attending Hogwarts)
* Completion Status: Some fanfictions are completed, others are works in progress, others are simply abandoned. Some users refuse to read incomplete works, as WIPs or abandoned fics sometimes stop just before a key plot point is resolved, leading to an unsatisfactory experience.
* Language: what language is the story written in?
* Characters: what characters are involved in this story? Who do they "pair off" with?

### Users
There are a number of features about users that may prove interesting to an external observer.

* Bookmarks: a list of all the fics a user has saved for later, or wishes to recommend to others
* Works: a list of all the stories the author has written or is in the process of writing
* Biography: a set of user characteristics like join date, user ID, name, and any pseudonyms they may have

## Functions
`FanficReadeR` uses a small set of functions to generate this data about individual works and/or authors. 

### Author Data

| Function  | Inputs| Description |
| ------------- | ------------- | ------------- | 
| `GetAuthorInfo()`  | Author Name OR Profile Link  | Gathers basic biographical data about an author on AO3. Includes data like: Date joined, Number of stories, number of bookmarks, Author ID  |
| `GetAuthorWorks()`  | Author Name OR Profile Link  | Gathers data about all works generated by the author. Includes data like: Work title, completion status, user engagement (kudos, comments, bookmarks, hits), romantic pairings (M/M, F/M, F/F, Multi, etc)|
| `GetAuthorBookmarks()`  | Author Name OR Profile Link  | Gathers data about all works bookmarked by the author. The data is near-identical to the output of `GetAuthorWorks()`  |
| `GetAuthorAll()`  | Author Name OR Profile Link  | Applies the above three functions in a single call, output as a list  |

### Works Data

| Function  | Inputs| Description |
| ------------- | ------------- | ------------- | 
| `GetWorksInfo()`  | Work Link OR Chapter Link  | Gathers basic summary data about the work in question. Includes data like: Work title, completion status, user engagement (kudos, comments, bookmarks, hits), romantic pairings (M/M, F/M, F/F, Multi, etc) |
| `GetChapterIndex()`  | Work Link OR Chapter Link  | Creates an index of all chapters in the relevant work. Lists their names, chapter order, and provides a URL|
| `GetComments`  | Work Link OR Chapter Link  | Gathers all comments on the relevant work. For each comment, this function also tells you which user made the comment, if that user was the author, when the comment was made, what chapter the comment was made on. The large nature of the data that this can generate means that I have added a few extra options to the function: 1) `keep.text = TRUE` is the default, and preserves the original text of the comment in the output, 2) `excl.author = FALSE` is the default, and removes comments made by the author on their own work|

Please note that many of these functions may take a while to run. Because this is scraping HTML web pages from AO3, I needed to add a `Sys.sleep(2)` to the functions that scrape multiple web pages in quick succession, so as not to be flagged as a bot.

## Examples
I have decided not to display an example of the functions working on GitHub, as that may violate the privacy of some random AO3 user. Included in this repository are two .rmd documents - `AuthorInfoTest.rmd` and `WorksInfoTest.rmd` - which contain versions of the functions you can modify for yourself, as well as some basic tests of the function that show how to call them using real users. I have, however, written down some example function calls for your reference.

### Basic Call Examples

```r
GetAuthorAll("SEthanMilne")
# GetAuthorAll("https://archiveofourown.org/users/SEthanMilne") ## Links can be used too

GetChapterIndex("https://archiveofourown.org/works/XXXXX") ## Work Link
# GetChapterIndex("https://archiveofourown.org/works/XXXXX/chapters/XXXXX") ## Chapter Link

GetComments("https://archiveofourown.org/works/XXXXX", keep.text = FALSE, excl.author = TRUE) ## Work Link
# GetComments("https://archiveofourown.org/works/XXXXX/chapters/XXXXX") ## Chapter Link
```

### Advanced Call Examples
A more complex use case of these functions might be scraping all comments received by an author, using both the AuthorInfo data AND comments data. I've done this myself on a set of users (to make sure this package worked at all), and an anonymized version of that code can be found below:

```r
works <- GetAuthorWorks("SEthanMilne") ## Get a df of all works

urls <- works$work_url ## extract a vector of URLs from the above call
work_name <- works$Titles ## Extract a vector of work names from the above call

all_comments <- list()

for (i in 1:length(urls)){

    ## Generate a list of all works by an author and associated comments

    all_comments[[i]] <- GetComments(urls[i], keep.text = FALSE, excl.author = TRUE) %>%
                    mutate(work = work_name[i]) ## add a column for the work name

}
```



## Installation

You can install `FanficReadeR` with the following code:

```r
install.packages("devtools") # if you have not installed "devtools" package
devtools::install_github("SEthanMilne/FanficReadeR")
```

## Citation

If you use this package for academic purposes, I ask that you cite me using the below information:

```r
  Ethan Milne (2021). FanficReadeR: A webscraper for gathering public data on AO3. R package version
  0.1.0.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {FanficReadeR: A webscraper for gathering public data on AO3},
    author = {Ethan Milne},
    year = {2021},
    note = {R package version 0.1.0},
  }
```

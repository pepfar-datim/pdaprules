% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3_list_bucket_items.R
\name{s3_list_bucket_items}
\alias{s3_list_bucket_items}
\title{s3_list_bucket_items(bucket)}
\usage{
s3_list_bucket_items(bucket, prefix = NULL, filter_parquet = FALSE)
}
\arguments{
\item{bucket}{your aws bucket}

\item{prefix}{a specific directory inside your bucket}

\item{filter_parquet}{default is FALSE and filters for txt, xlsx ,csv and xlsx. If TRUE returns parquet}
}
\description{
Standard function for connecting to S3, returns a dataframe of files.
}

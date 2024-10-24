% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_peaks.R
\name{detect_peaks}
\alias{detect_peaks}
\title{Detect Peaks in 1D or 2D Data with Adaptive Noise Threshold
This function detects peaks in a numeric vector (1D) or numeric matrix (2D).
If no noise_threshold is provided, it is set adaptively based on the data.}
\usage{
detect_peaks(data, noise_threshold = NULL, min_distance = 1)
}
\arguments{
\item{data}{A numeric vector (1D) or matrix (2D) representing the data.}

\item{noise_threshold}{A numeric value specifying the minimum height a peak
must have to be considered significant. If NULL, it will be calculated
as a fraction of the standard deviation of the data.}

\item{min_distance}{An integer specifying the minimum distance between peaks.
This helps avoid detecting multiple close peaks in noisy regions. Defaults to 1.}
}
\value{
A PeakData object with positions and heights of detected peaks for 1D data,
        or a list of PeakData objects for each row of 2D data.
}
\description{
Detect Peaks in 1D or 2D Data with Adaptive Noise Threshold
This function detects peaks in a numeric vector (1D) or numeric matrix (2D).
If no noise_threshold is provided, it is set adaptively based on the data.
}

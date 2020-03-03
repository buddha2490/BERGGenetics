\name{BERGSamples}
\alias{BERGSamples}
\docType{data}
\title{
Metadata file for all genotyped samples in CPS-II
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("BERGSamples")}
\format{
  A list with 32 data frames
  \describe{
    \item{\code{ICOGS}}{Sample data for ICOGS}
    \item{\code{BRCA_CGEMS}}{Sample data for CGEMS breast}
    \item{\code{ERNEG}}{Sample data for ER- breast cancer GWAS (BPC3)}
    \item{\code{BCAC}}{Sample data for BCAC  GWAS (breast Oncoarray)}
    \item{\code{BRCA_TAQ}}{Sample data for Breast cancer taqman data}
    \item{\code{AAPC}}{Sample data for African American Prostate Cancer GWAS (AAPC)}
    \item{\code{AGGPRCA}}{Sample data for Aggressive Prostate GWAS}
    \item{\code{PRCA_TAQ_CGF1}}{Sample data for CGF Prostate Taqman data - 1st wave}
    \item{\code{PRCA_TAQ_CGF2}}{Sample data for CGF Prostate Taqman data - 2nd wave}
    \item{\code{PRCA_TAQ_CGF3}}{Sample data for CGF Prostate Taqman data - 3rd wave}
    \item{\code{PRCA_TAQ_USC}}{Sample data for USC Prostate Taqman data}
    \item{\code{PRCA_CGEMS1}}{Sample data for CGEMS prostate - 1st wave}
    \item{\code{PRCA_CGEMS2_BLOOD}}{Sample data for CGEMS prostate - 2nd wave (blood)}
    \item{\code{PRCA_CGEMS2_BUCCAL}}{Sample data for CGEMS prostate - 2nd wave (buccal)}
    \item{\code{BLADDER}}{Sample data for Bladder GWAS}
    \item{\code{NHL}}{Sample data for NHL Gwas}
    \item{\code{CORECT}}{Sample data for CORECT GWAS}
    \item{\code{GECCO_EXCOME}}{Sample data for GECCO Exome}
    \item{\code{GECCO_ONCOARRAY}}{Sample data for GECCO (oncoarray GWAS)}
    \item{\code{E2C2}}{Sample data for E2C2 (Endometrium)}
    \item{\code{GLIOMA}}{Sample data for Glioma GWAS}
    \item{\code{LUNG_COMBINED}}{Sample data for Lung cancer (BLK GWAS)}
    \item{\code{LUNG_1M_CONTROL}}{Sample data for Lung cancer (BLK GWAS)}
    \item{\code{LYM_SUPER}}{Sample data for Lymphoma super controls (GWAS)}
    \item{\code{TGS}}{Sample data for NCI Total GWAS set}
    \item{\code{NICOTINE}}{Sample data for Nicotine SNPs}
    \item{\code{PANSCAN1}}{Sample data for PANSCAN-1 (GWAS)}
    \item{\code{PANSCAN2}}{Sample data for PANSCAN-2 (GWAS)}
    \item{\code{PRCA_ONCOARRAY}}{Sample data for Prostate GWAS (Oncoarray)}
    \item{\code{RCC_GWAS}}{Sample data for Renal Cell Carcinoma GWAS}
    \item{\code{RENAL_GWAS}}{Sample data for Renal GWAS}
    \item{\code{Summary}}{Summary table listing all our samples and collaborations}
  }
}
\details{
Each of these data frames include all ID conversions to merge the genetics data with our larger cohort data.  When possible, case-control information has also been included.
}

\examples{
data(BERGSamples)

}
\keyword{datasets}

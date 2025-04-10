# jobs robbed by robot
# luddite rebellion looming
# blue, due to red pill
library(httr)

tf <- tempfile()

this_url <-	"ftp://ftp.census.gov/programs-surveys/cps/datasets/2024/march/asecpub24sas.zip"

GET( this_url , write_disk( tf ) , progress() , timeout( 999 ) )

unzipped_files <- unzip( tf , exdir = tempdir() )
library(haven)

four_tbl <- lapply( unzipped_files , read_sas )

four_df <- lapply( four_tbl , data.frame )

four_df <- lapply( four_df , function( w ){ names( w ) <- tolower( names( w ) ) ; w } )

household_df <- four_df[[ grep( 'hhpub' , basename( unzipped_files ) ) ]]
family_df <- four_df[[ grep( 'ffpub' , basename( unzipped_files ) ) ]]
person_df <- four_df[[ grep( 'pppub' , basename( unzipped_files ) ) ]]
repwgts_df <- four_df[[ grep( 'repwgt' , basename( unzipped_files ) ) ]]
household_df[ , 'hsup_wgt' ] <- household_df[ , 'hsup_wgt' ] / 100
family_df[ , 'fsup_wgt' ] <- family_df[ , 'fsup_wgt' ] / 100
for ( j in c( 'marsupwt' , 'a_ernlwt' , 'a_fnlwgt' ) ) person_df[ , j ] <- person_df[ , j ] / 100
names( family_df )[ names( family_df ) == 'fh_seq' ] <- 'h_seq'
names( person_df )[ names( person_df ) == 'ph_seq' ] <- 'h_seq'
names( person_df )[ names( person_df ) == 'phf_seq' ] <- 'ffpos'

hh_fm_df <- merge( household_df , family_df )
hh_fm_pr_df <- merge( hh_fm_df , person_df )
cps_df <- merge( hh_fm_pr_df , repwgts_df )

stopifnot( nrow( cps_df ) == nrow( person_df ) )
# cps_fn <- file.path( path.expand( "~" ) , "CPS" , "this_file.rds" )
# saveRDS( cps_df , file = cps_fn , compress = FALSE )
# cps_df <- readRDS( cps_fn )
library(survey)
	
cps_design <-
	svrepdesign(
		weights = ~ marsupwt ,
		repweights = "pwwgt[1-9]" ,
		type = "Fay" ,
		rho = ( 1 - 1 / sqrt( 4 ) ) ,
		data = cps_df ,
		combined.weights = TRUE ,
		mse = TRUE
	)
cps_design <- 
	update( 
		cps_design , 

		one = 1 ,

		a_maritl = 
			factor( 
				a_maritl , 
				labels = 
					c( 
						"married - civilian spouse present" ,
						"married - AF spouse present" ,
						"married - spouse absent" ,
						"widowed" ,
						"divorced" , 
						"separated" , 
						"never married"
					)
			) ,
			
		state_name =
			factor(
				gestfips ,
				levels = 
					c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 
					11L, 12L, 13L, 15L, 16L, 17L, 18L, 
					19L, 20L, 21L, 22L, 23L, 24L, 25L, 
					26L, 27L, 28L, 29L, 30L, 31L, 32L, 
					33L, 34L, 35L, 36L, 37L, 38L, 39L, 
					40L, 41L, 42L, 44L, 45L, 46L, 47L, 
					48L, 49L, 50L, 51L, 53L, 54L, 55L, 
					56L) ,
				labels =
					c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
					"Colorado", "Connecticut", "Delaware", "District of Columbia", 
					"Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
					"Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
					"Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
					"Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
					"New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
					"Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
					"South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
					"Washington", "West Virginia", "Wisconsin", "Wyoming")
			) ,

		male = as.numeric( a_sex == 1 )
	)
sum( weights( cps_design , "sampling" ) != 0 )

svyby( ~ one , ~ state_name , cps_design , unwtd.count )
svytotal( ~ one , cps_design )

svyby( ~ one , ~ state_name , cps_design , svytotal )
svymean( ~ ptotval , cps_design )

svyby( ~ ptotval , ~ state_name , cps_design , svymean )
svymean( ~ a_maritl , cps_design )

svyby( ~ a_maritl , ~ state_name , cps_design , svymean )
svytotal( ~ ptotval , cps_design )

svyby( ~ ptotval , ~ state_name , cps_design , svytotal )
svytotal( ~ a_maritl , cps_design )

svyby( ~ a_maritl , ~ state_name , cps_design , svytotal )
svyquantile( ~ ptotval , cps_design , 0.5 )

svyby( 
	~ ptotval , 
	~ state_name , 
	cps_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE 
)
svyratio( 
	numerator = ~ moop , 
	denominator = ~ ptotval , 
	cps_design 
)
sub_cps_design <- subset( cps_design , a_age %in% 18:64 )
svymean( ~ ptotval , sub_cps_design )
this_result <- svymean( ~ ptotval , cps_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ ptotval , 
		~ state_name , 
		cps_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( cps_design )
svyvar( ~ ptotval , cps_design )
# SRS without replacement
svymean( ~ ptotval , cps_design , deff = TRUE )

# SRS with replacement
svymean( ~ ptotval , cps_design , deff = "replace" )
svyciprop( ~ male , cps_design ,
	method = "likelihood" )
svyttest( ptotval ~ male , cps_design )
svychisq( 
	~ male + a_maritl , 
	cps_design 
)
glm_result <- 
	svyglm( 
		ptotval ~ male + a_maritl , 
		cps_design 
	)

summary( glm_result )
count_covered <- svytotal( ~ as.numeric( cov == 1 ) , cps_design )

stopifnot( round( coef( count_covered ) , -5 ) == 305200000 )

stopifnot(
	round( coef( count_covered ) - confint( count_covered , level = 0.9 )[1] , -3 ) == 704000
)

share_covered <- svymean( ~ as.numeric( cov == 1 ) , subset( cps_design , cov > 0 ) )

stopifnot( round( coef( share_covered ) , 3 ) == 0.920 )

stopifnot(
	round( coef( share_covered ) - confint( share_covered , level = 0.9 )[1] , 3 ) == 0.002
)

library(convey)
cps_design <- convey_prep( cps_design )

cps_household_design <- subset( cps_design , a_exprrp %in% 1:2 )

svygini( ~ htotval , cps_household_design )
library(srvyr)
cps_srvyr_design <- as_survey( cps_design )
cps_srvyr_design %>%
	summarize( mean = survey_mean( ptotval ) )

cps_srvyr_design %>%
	group_by( state_name ) %>%
	summarize( mean = survey_mean( ptotval ) )

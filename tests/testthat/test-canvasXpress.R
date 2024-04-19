###### CanvasXpress & utilities
test_that("testing attributes injection", { 

        x_factors <- list("factor1"= c("A", "B", "B", "A"), 
                 		  "factor2"= c("Y", "Y", "N", "N"))
        
        z_factors <- list("factor3" = c(TRUE, TRUE, TRUE, FALSE, FALSE),
                   		  "factor4" = c(TRUE, FALSE, TRUE, FALSE, TRUE))

        #Testing with no additional added factors (it should return the same json)
  		vars<- c("gen1", "gen2", "gen3", "gen4")
		smps <- c("liver", "brain", "cerebellum")
        vals <-  data.frame(gen1 = c(20, 13, 15),
							 gen2 = c(40, 60, 30),
      						 gen3 = c(100, 85, 12),
            				 gen4 = c(85, 10, 41)) 
	      
        smp_attr <- list("nerv" =  c("no", "yes", "yes"), "pcr" = c("true", "true", "false"))
        var_attr <- list("type" = c("miRNA", "miRNA", "mRNA", "mRNA"), "type2" = c("tRNA" , "tRNA" , "ncRNA", "ncRNA"))
		cvX <- canvasXpress_obj$new(vars = vars, smps = smps, vals = vals, smp_att = smp_attr, var_att = var_attr)

		cvX_returned <- canvasXpress_obj$new(vars = vars, smps = smps, vals = vals, smp_att = smp_attr, var_att = var_attr)

		options <- list()
       
        cvX_returned$inject_attributes(options, "x")
        cvX_returned$inject_attributes(options, "z")
       	testthat::expect_equal(cvX$data_structure, cvX_returned$data_structure)


        #Testing with additional added factors
        cvX$data_structure[["x"]] <- update_options(cvX$data_structure[['x']], x_factors)
        cvX$data_structure[["z"]] <- update_options(cvX$data_structure[['z']], z_factors)


        options$inject_smp_attr <- x_factors
        options$inject_var_attr <- z_factors
        
        cvX_returned$inject_attributes(options, "x")
        cvX_returned$inject_attributes(options, "z")
        
       	testthat::expect_equal(cvX$data_structure, cvX_returned$data_structure)
})

test_that("test data segregation", {
		
        variables_to_segregate <- list("var" = c("nerv", "pcr"), "smp" = c("type", "type2")) 
        expected <- "table1.segregateVariables(['nerv','pcr']);\n\ntable1.segregateSamples(['type','type2']);\n\n"
        cvX <- canvasXpress_obj$new()
        cvX$segregate_data("table1", variables_to_segregate)
        testthat::expect_equal(expected, cvX$extracode)
})

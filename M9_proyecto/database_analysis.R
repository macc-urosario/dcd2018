setwd("C:/Users/diant/OneDrive/Documentos/Proyecto Ciencia de datos")
getwd()

cancer <- read.delim("risk_factors_cervical_cancer.csv",
                     sep = ",",
                     colClasses = c("numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "factor",
                                    "numeric",
                                    "numeric",
                                    "factor",
                                    "numeric",
                                    "factor",
                                    "numeric",
                                    "factor",
                                    "numeric",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "numeric",
                                    "numeric",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor"),
                     na.strings = c("NA", "?"))
head(cancer)
names(cancer) <- c("edad", "parejas_sexuales", "primera_relacion", "numero_embarazos",
                   "fuma", "anios_fumando", "paquetes_anio", "anticonceptivos",
                   "anios_anticonceptivo", "diu", "diu_anos", "ets", "ets_numero",
                   "ets_condilomatosis", "ets_condilomatosis_cervical", "ets_condilomatosis_vaginal", 
                   "ets_condilomatosis_vulvo_perineal", "ets_sifilis", "ets_enfermedad_inflamatoria_pelvica",
                   "ets_herpes_genital", "ets_molluscum_contagiosum", "ets_sida", "ets_vih", "ets_hepatitis_b",
                   "ets_vph", "numero_diagnostico", "tiempo_desde_primer_diagnostico", "tiempo_desde_ultimo_diagnostico",
                   "diag_cancer", "diag_neoplasia_cervical_intraepitelial", "vph", "diag", "hinselmann",
                   "schiller", "citologia", "biopsia")



names(cancer)
unique(cancer$anticonceptivos)
write.csv(cancer, "cancer.csv", row.names = F, na = "")

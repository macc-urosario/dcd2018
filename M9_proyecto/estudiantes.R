estudiantes <- read.csv("Proyecto Ciencia de datos/xAPI-Edu-Data.csv")
names(estudiantes) <- c("genero",
                        "nacionalidad",
                        "lugar_nacimiento",
                        "nivel_educativo",
                        "grado",
                        "seccion",
                        "tema",
                        "semestre",
                        "familiar_responsable",
                        "participacion_mano",
                        "participacion_recursos",
                        "participacion_anuncios",
                        "participacion_grupos",
                        "encuesta_familiar",
                        "satisfaccion_familiar",
                        "dias_ausencia")

estudiantes
write.csv(estudiantes, "Proyecto Ciencia de datos/estudiantes.csv")

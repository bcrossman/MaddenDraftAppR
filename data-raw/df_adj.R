## code to prepare `df_adj` dataset goes here
df_adj <- 
  structure(list(term = c("(Intercept)", "devTrait1", "devTrait2", 
                          "devTrait3", "LOLB", "LT", "LE", "DT", "SS", "MLB", "K", "RG", 
                          "TE", "ROLB", "LG", "WR", "FS", "CB", "HB", "P", "QB", "RE", 
                          "RT", "C", "FB"), estimate = c(-5541032.27874081, 574644.706954128, 
                                                         2495126.24885999, 4900937.25003699, 93406.6706319988, 98438.5363601197, 
                                                         100658.378564114, 92846.4057296973, 87103.9187276402, 99229.5897950681, 
                                                         75732.1852241471, 89186.62618189, 87712.4047499873, 97518.0621248342, 
                                                         92362.7552084079, 94841.2106631303, 92930.9670152674, 103178.322533558, 
                                                         88596.1873994102, 81404.3378082528, 108281.279469531, 96848.9121614093, 
                                                         96044.5749079337, 90691.5935473101, 80709.3577991745), std.error = c(563721.988089133, 
                                                                                                                              120827.706609753, 217085.478568061, 264761.915094373, 8093.09317235907, 
                                                                                                                              8542.74813129808, 8030.70508832456, 7969.95131920556, 8110.31482219515, 
                                                                                                                              8178.73196949759, 8570.93671855391, 8317.52360156512, 8179.26929750183, 
                                                                                                                              8250.60145007519, 8561.87475944024, 7811.72906647841, 8258.57428914781, 
                                                                                                                              7864.02946740281, 7930.13951939172, 8635.38437473013, 9218.44339205434, 
                                                                                                                              8225.37502928083, 8389.57736775477, 8427.16576403716, 9623.52002083157
                                                         ), statistic = c(-9.82937049789991, 4.75590179668065, 11.493750136206, 
                                                                          18.5107334953748, 11.5415291338814, 11.5230526344878, 12.5341893964523, 
                                                                          11.6495574453461, 10.739893658538, 12.1326374510306, 8.8359286401224, 
                                                                          10.7227379751718, 10.7237457968008, 11.8195094884804, 10.7876788441188, 
                                                                          12.1408730200477, 11.2526646563419, 13.1202868658164, 11.1720843224466, 
                                                                          9.4268343221024, 11.7461565759423, 11.7744068588538, 11.4480826265552, 
                                                                          10.7618143616369, 8.38667739293593), p.value = c(2.62345330106354e-20, 
                                                                                                                           2.88637672780846e-06, 3.54078629004587e-26, 6.63063750348262e-54, 
                                                                                                                           2.36722607691922e-26, 2.76628602609554e-26, 4.72322461101351e-30, 
                                                                                                                           9.49992262094378e-27, 1.83548874927485e-23, 1.53356958945343e-28, 
                                                                                                                           4.80270342663965e-17, 2.11109796649318e-23, 2.09382565081933e-23, 
                                                                                                                           2.24243559176677e-27, 1.24242669962033e-23, 1.4285544622673e-28, 
                                                                                                                           2.66982267487136e-25, 2.73004561184434e-32, 5.22224244762895e-25, 
                                                                                                                           5.82236909209609e-19, 4.18611063831204e-27, 3.29222914612405e-27, 
                                                                                                                           5.1991663455396e-26, 1.53479584976289e-23, 1.22258747932475e-15
                                                                          ), adjustment = c(-51.1725785462298, 5.30696266029834, 23.0430067051627, 
                                                                                            45.261168634566, 0.862629912479771, 0.90910023267521, 0.929600934318827, 
                                                                                            0.857455750288059, 0.804422695726924, 0.916405774674934, 0.399402386037169, 
                                                                                            0.823656929607914, 0.810042189930614, 0.900599462830276, 0.852989137742854, 
                                                                                            0.875878186217941, 0.858236691240959, 0.952873137803954, 0.818204105395157, 
                                                                                            0.451785887708866, 1, 0.894419724590175, 0.886991503780299, 0.837555614337102, 
                                                                                            0.745367603657523)), row.names = c(NA, -25L), class = c("tbl_df", 
                                                                                                                                                    "tbl", "data.frame"))

usethis::use_data(df_adj, overwrite = TRUE)


library(patchwork)

p5
p6
p8
p9
p10
p12

p14
p15

painel <- (p5 + p10) / (p12_mod + p9) / (p6 + p8)

ggsave("img/painel", plot = painel, width = 16, height = 8, dpi = "retina")
# To do:
# 1. Ta bort alla banker, investmentbolag och fastighetsbolag som inte �r
# top 2 i resp kategori efter f�rsta sorteringen.
# 2. Beh�ll bara B aktien om det finns flera.

# Loading the rvest package
library(rvest)
library(dplyr)

# L�ser in aktier som vi vill kunna filtrera ut
banker <- read.csv(file = "banker.txt", sep = "\n", stringsAsFactors = FALSE)
fastigheter <- read.csv(file = "fastigheter.txt", sep = "\n", stringsAsFactors = FALSE)
investmentbolag <- read.csv(file = "investmentbolag.txt", sep = "\n", stringsAsFactors = FALSE)
abaktier <- read.csv(file = "abaktier.txt", sep = "\n", stringsAsFactors = FALSE)

banker$bank <- 1
banker$banker <- tolower(banker$banker) 

fastigheter$fastigheter <- 1
fastigheter$fastighet <- tolower(fastigheter$fastighet)

investmentbolag$invest <- 1
investmentbolag$investment <- tolower(investmentbolag$investment)

abaktier$abaktier <- tolower(abaktier$abaktier)
abaktier$abaktie <- 1

# Specifirerar vilken hemsida vi vill l�sa data fr�n
url <- 'https://www.nordnet.se/mux/web/marknaden/kurslista/aktier.html?marknad=Sverige&lista=1_1&large=on&sektor=0&subtyp=key_ratios&sortera=&sorteringsordning='

# L�ser in HTML kod Fr�n hemsida
webpage <- read_html(url)

# Plockar ut de olika variablerna som vi vill ha (JEK, DA, P/E)
aktie_html <- html_nodes(webpage, '.underline')
jek_html <- html_nodes(webpage,'td:nth-child(9)')
pe_html <- html_nodes(webpage, '.text+ td')
da_html <- html_nodes(webpage, 'td:nth-child(8)')

# Converting the title data to text
aktie <- html_text(aktie_html)
jek <- html_text(jek_html)
pe <- html_text(pe_html)
da <- html_text(da_html)



alles <- cbind(pe, jek, da)
df_alles <- as.data.frame(alles)
df_alles <- df_alles %>% slice(-1)
df_alles <- cbind(aktie, df_alles)
names(df_alles) <- c("Aktie", "PE", "EK", "DA")
head(df_alles)

df_alles$PE<- gsub(',', '.', df_alles$PE)
df_alles$EK<- gsub(',', '.', df_alles$EK)
df_alles$DA<- gsub(',', '.', df_alles$DA)

clean_df <- df_alles %>% mutate(Aktie = tolower(Aktie),
                                PE = as.numeric(as.character(PE)),
                                 EK = as.numeric(as.character(EK)),
                                 DA = as.numeric(as.character(DA)))


# Filtrera ut aktier som inte m�ter villkoren f�r k�p
tmp_kop <- clean_df %>%
  mutate(EP = (1/PE)*100) %>% 
  filter(PE > 0, DA > 0) %>% 
  left_join(abaktier, by = c("Aktie" = "abaktier")) %>% 
  left_join(banker, by = c("Aktie" = "banker")) %>% 
  left_join(fastigheter, by = c("Aktie" = "fastighet")) %>% 
  left_join(investmentbolag, by = c("Aktie" = "investment")) %>% 
  filter(is.na(abaktie)) %>% 
  arrange(desc(EP)) %>% mutate(a = row_number()) %>% 
  mutate(bank_rank = row_number(bank),
         fast_rank = row_number(fastigheter),
         inve_rank = row_number(invest)) %>% 
  filter((bank_rank<=2|is.na(bank_rank)), 
         (fast_rank<=2|is.na(fast_rank)),
         (inve_rank<=2|is.na(inve_rank))) %>% 
  arrange(EK) %>% mutate(b = row_number()) %>% 
  mutate(c = a + b) %>% arrange(c, DA)
#     

# Analysen:
# Steg 1: EP-tal, h�gst EP tal h�gst upp
# Steg 2: Ej sorterat p� rad
# 3. Ny kolumn med 1 f�r det h�gsta EP talet 2 f�r n�sta osv.
# 4. Totalt eget kapital / Aktie. H�gst h�gst upp
# 5. Ny kolumn med 1 f�r h�gsta EK 2 f�r andra osc
# 6. ny kolumn c. Summera rank (a+b)
# 7. Sortera s� att komumn c har l�gst v�rde h�gst upp
# 8. Sortera p� DA av de 20 h�gsta bolagen. V�lj ut de 10 och k�p dessa.
# f�rvalta bolag 11-20 (men k�p en) S�lj i mitten av april
# S�lj andra bolag

# ej pref
# ej huvudstad C
 
# ej negativt pe
# ej 0 i DA
# ej banker (de 2 h�gsta efter f�rsta sorteringen)
# ej investmentbolag (de 2 h�gsta efter f�rsta sorteringen)
# ej fastighetsbolag (de 2 h�gsta efter f�rsta sorteringen)
# 






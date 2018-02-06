# To do:
# 1. Ta bort alla banker, investmentbolag och fastighetsbolag som inte �r
# top 2 i resp kategori efter f�rsta sorteringen

# Loading the rvest package
library(rvest)
library(dplyr)

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

clean_df <- df_alles %>% mutate(PE = as.numeric(as.character(PE)),
                                 EK = as.numeric(as.character(EK)),
                                 DA = as.numeric(as.character(DA)))


# Filtrera ut aktier som inte m�ter villkoren f�r k�p
tmp_kop <- clean_df %>%
    mutate(Aktie = tolower(Aktie), 
           EP = (1/PE)*100) %>% 
  filter(PE > 0, DA > 0) %>% 
    arrange(desc(EP)) %>% mutate(a = row_number()) %>% 
    arrange(EK) %>% mutate(b = row_number()) %>% 
    mutate(c = a + b) %>% arrange(c, DA)
    

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





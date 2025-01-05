###################
# global.R
# 
# tabelas utilizadas e outros objetos
###################

library(sf)
library(readr)

# Importar dados
sinaflor_dof_agregada <- readr::read_csv("dados/sinaflor_dof_agregada.csv")
car_autex_sinaflor <- sf::st_read("dados/CAR_autex_2020a2022_risco.shp")

# Ler sinaflor_dof_agregada como objeto SF
sinaflor_dof_agregada_sf <- sinaflor_dof_agregada %>%
  sf::st_as_sf(wkt = "localizacao_autex", crs = 4674)

# Aplicando a transformação para epsg 4674
sinaflor_dof_agregada_sf <- sf::st_transform(sinaflor_dof_agregada_sf, crs = 4674)
car_autex_sinaflor <- sf::st_transform(car_autex_sinaflor, crs = 4674)


# Descrição do app
metodologia_html <- "
  <h3>Conheça a Solução do <b>Time Siwsi</b> idealizada para o IPAAM</h3>
  <p><b>Problema:</b> <i>Como identificar e validar irregularidades no transporte de madeira em relação às autorizações concedidas?</i></p>
  <p><b>Solução:</b> Sistema de Engenharia de Dados com validações de regras de negócio utilizando análise geoespacial.</p>
  
  <h4>Como funciona?</h4>
  <p>Automatizamos a extração de dados de APIs do IBAMA e SINAFLOR para:</p>
  <ol>
    <li><b>Identificar autorizações de exploração florestal</b> e declarações do corte no sistema.</li>
    <li><b>Cruzamento de dados:</b> Comparar as quantidades de madeira autorizadas para extração com a quantidade de madeira transportada dos pátios, durante o período de validade da autorização.</li>
    <li><b>Identificação de irregularidades:</b> Detectar transportes que excedem as autorizações concedidas, validar utilizando análise geoespacial e apresentar os resultados em relatórios interativos.</li>
  </ol>
  
  <h3>Metodologia em Detalhe</h3>
  
  <h4>1. Cruzamento de Dados e Identificação de Quantidades Suspeitas</h4>
  <p>Esta análise foi desenvolvida para avaliar os volumes transportados em relação aos volumes autorizados pelo sistema Autex.</p>
  <p>Cada transporte foi classificado em categorias de risco com base nos seguintes critérios:</p>
  <ul>
    <li><b>Sem Risco:</b> Volume transportado entre 75% e 85% do volume autorizado.<br>
      <i>Interpretação:</i> Transportes proporcionais e adequados, sugerindo uma operação dentro dos limites seguros e legais, com margem de segurança. 
      <i>Nota:</i> Este valor foi mencionado como referência em uma palestra de Márcio, representando operações legais usuais.</li>
    <li><b>Médio Risco:</b> Volume transportado entre 95% e 100% do volume autorizado.<br>
      <i>Interpretação:</i> Transportes próximos ao limite autorizado, representando um risco moderado de violação caso ocorram erros ou ajustes na medição.</li>
    <li><b>Alto Risco:</b> Volume transportado maior que o volume autorizado.<br>
      <i>Interpretação:</i> Indica potenciais violações legais ou atividades irregulares, sugerindo abuso das permissões concedidas.</li>
    <li><b>Fora do Escopo:</b> Casos que não se enquadram nos critérios acima.<br>
      <i>Interpretação:</i> Representa transportes muito abaixo do limite autorizado, fora das faixas de interesse analisadas.</li>
  </ul>
  
  <h4>2. Validação Geoespacial</h4>
  <p>Os dados foram georreferenciados para facilitar a visualização espacial, utilizando coordenadas no formato WKT 
     convertidas para o sistema CRS WGS84.</p>
  
  <h4>3. Como o Time Swisi vislumbra um MVP</h4>
  <p>O MVP integra os seguintes elementos:</p>
  <ul>
    <li>Automatização da coleta de dados em APIs do IBAMA e SINAFLOR.</li>
    <li>Análise comparativa e categorização de riscos nos volumes transportados.</li>
    <li>TODO: Validações com análise geoespacial, tendo posse dos dados de inventarios do IPAAM.</li>
  </ul>
  
  <p>Com essa solução, visamos apoiar o IPAAM na identificação e validação de irregularidades, promovendo transparência e eficiência na gestão florestal.</p>
"
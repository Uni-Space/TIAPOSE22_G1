# Bem-vindo ao Repositório TIAPOSE22_G1

No âmbito da unidade curricular de Técnicas de Inteligência Artificial na Previsão e Otimização em Sistemas Empresariais foi proposto o desenvolvimento de um projeto de suporte à decisão para gerir um espaço comercial, utilizando técnicas de previsão e otimização num problema realista. 

Para tal, foi-nos disponibilizado um conjunto de dados relativos a valores totais diários de entradas de pessoas no respetivo espaço comercial. Este projeto tem como principais objetivos prever o valor diário de entradas no espaço comercial (nas diferentes dimensões das cinco séries temporais), e para um determinado dia pretende-se definir uma estratégia de marketing para uma semana.

Este repositório está dividido em três diretorias principais:
 - prediction;
 - optimization;
 - interface.

## Diretoria "prediction"
Aqui foi realizada a previsão para todas as séries temporais (all, female, male, young e adult) de entradas na loja.
 
Foram realizadas três tipos de análise: Univariada, Multivariada e Híbrida.

No total foram obtidos 10 020 resultados, os quais estão presentes na diretoria "[/prediction/models/results](/prediction/models/results/)" em ficheiros excel.

## Diretoria "optimization"
Aqui foi definida a melhor estratégia de marketing para uma semana tendo em conta 3 objetivos e as previsões para todas as séries temporais (all, female, male, young e adult).

Para cada um dos modelos utilizados nesta fase, foi atribuído um jupyter notebook criado com o propósito de testar todos os objetivos com diferentes parâmetros para cada modelo.

De forma a escolher os melhores modelos para cada objetivo de otimização, desenvolvemos um [jupyter notebook](/optimization/evaluation.ipynb) onde começamos por criar uma função, denominada optimize, que contém todos os modelos e recebe como parâmetros o nome do modelo, o objetivo de avaliação, o número de pesquisas e, caso aplicado, o número de população e qual a estratégia a usar no objetivo 2 (Death Penalty ou Repair). Esta função retorna uma lista com o resultado da função de avaliação e o tempo de execução do modelo. Todos os resultados obtidos estão na diretoria "[/optimization/results](/optimization/results/)".

## Diretoria "interface"
Aqui encontra-se o produto final. É uma aplicação Shiny que permite testar todos os modelos utilizados na fase de previsão e otimização e ler detalhes do projeto.

**A aplicação encontra-se disponível para demonstração em:** [uni-space.shinyapps.io/tiapose](https://uni-space.shinyapps.io/tiapose/)

## Tecnologia Utilizada
 - **R**;
 - **Jupyter Notebook**;
 - **Shiny**.

## Contactos
 - [Carlos Gonçalves](https://www.linkedin.com/in/carlosafgoncalves/);
 - [Inês Ferreira](https://www.linkedin.com/in/in%C3%AAs-carvalho-ferreira/);
 - [João Gonçalves](https://github.com/Ines0603);
 - [Maria Pereira](https://github.com/mariajcpereira);
 - [Rui Gomes](https://www.linkedin.com/in/ruigomes99/).

## Agradecimentos
 - [Paulo Cortez](http://www3.dsi.uminho.pt/pcortez/Home.html);

## License
 - [MIT](https://choosealicense.com/licenses/mit/)
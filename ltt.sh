sed 's/LETTERS/letters/g' <A_AltPendientesTendMensGrf.R >A_AltPendientesTendMensGrf-nn.R
sed 's/LETTERS/letters/g' <AltPendientesTendMensGrf.R >AltPendientesTendMensGrf-nn.R
sed 's/LETTERS/letters/g' <GraficosClimaAnual.R >GraficosClimaAnual-nn.R
sed 's/LETTERS/letters/g' <GraficosClima.R >GraficosClima-nn.R 
sed 's/LETTERS/letters/g' <PendientesTendAnualGrf.R >PendientesTendAnualGrf-nn.R
sed 's/LETTERS/letters/g' <test.R >test-nn.R
swap.sh A_AltPendientesTendMensGrf.R A_AltPendientesTendMensGrf-nn.R
swap.sh AltPendientesTendMensGrf.R AltPendientesTendMensGrf-nn.R
swap.sh GraficosClimaAnual.R GraficosClimaAnual-nn.R
swap.sh GraficosClima.R GraficosClima-nn.R 
swap.sh PendientesTendAnualGrf.R PendientesTendAnualGrf-nn.R
swap.sh test.R test-nn.R


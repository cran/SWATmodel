include $(R_HOME)/etc$(R_ARCH)/Makeconf
PROG =	../rswat2009.exe

SRCS =	addh.f albedo.f allocate_parms.f alph.f anfert.f apex_day.f apply.f \
	ascrv.f atri.f aunif.f autoirr.f aveval.f bacteria.f biozone.f \
	bmp_det_pond.f bmp_ri_pond.f bmp_sand_filter.f bmp_sed_pond.f \
	bmp_wet_pond.f bmpfixed.f bmpinit.f buffer.f burnop.f canopyint.f \
	caps.f carbon_new.f chkcst.f clgen.f clicon.f command.f conapply.f \
	confert.f crackflow.f crackvol.f curno.f dailycn.f decay.f depstor.f \
	distrib_bmps.f dormant.f drains.f dstn1.f ee.f eiusle.f enrsb.f \
	erfc.f estimate_ksat.f etact.f etpot.f expo.f fert.f filter.f filtw.f \
	finalbal.f gcycl.f getallo.f getallo2.f grass_wway.f graze.f grow.f \
	gw_no3.f gwmod.f gwnutr.f h2omgt_init.f harvestop.f harvgrainop.f \
	harvkillop.f header.f headout.f hhnoqual.f hhwatqual.f hmeas.f \
	hruaa.f hruallo.f hruday.f hrumon.f hrupond.f hrupondhr.f hruyr.f \
	hydroinit.f impnd_init.f impndaa.f impndday.f impndmon.f impndyr.f \
	irr_rch.f irr_res.f irrigate.f irrsub.f jdt.f killop.f lakeq.f \
	latsed.f layersplit.f lwqdef.f main.f modparm.f ndenit.f newtillmix.f \
	nfix.f nitvol.f nlch.f nminrl.f noqual.f npup.f nrain.f nup.f nuts.f \
	openwth.f operatn.f orgn.f orgncswat.f origtile.f ovr_sed.f \
	percmacro.f percmain.f percmicro.f pestlch.f pestw.f pesty.f pgen.f \
	pgenhr.f pkq.f plantmod.f plantop.f pmeas.f pminrl.f pminrl2.f pond.f \
	pondhr.f pothole.f potholehr.f print_hyd.f psed.f qman.f ran1.f \
	rchaa.f rchday.f rchinit.f rchmon.f rchuse.f rchyr.f reachout.f \
	readatmodep.f readbsn.f readchm.f readcnst.f readfcst.f readfert.f \
	readfig.f readfile.f readgw.f readhru.f readinpt.f readlup.f \
	readlwq.f readmgt.f readmon.f readops.f readparmfile.f readpest.f \
	readplant.f readpnd.f readres.f readrte.f readru.f readsdr.f \
	readsepticbz.f readseptwq.f readsno.f readsol.f readsub.f readswq.f \
	readtill.f readurban.f readwgn.f readwus.f readwwq.f readyr.f \
	reccnst.f recday.f rechour.f recmon.f recyear.f regres.f res.f \
	resbact.f resetlu.f reshr.f resinit.f resnut.f rewind_init.f rhgen.f \
	rootfr.f route.f routels.f routeunit.f routres.f rsedaa.f rseday.f \
	rsedmon.f rsedyr.f rtbact.f rtday.f rteinit.f rthmusk.f rthpest.f \
	rthr.f rthsed.f rtmusk.f rtout.f rtover.f rtpest.f rtsed.f \
	rtsed_bagnold.f rtsed_kodatie.f rtsed_Molinas_Wu.f rtsed_yangsand.f \
	ruallo.f sat_excess.f save.f saveconc.f sched_mgt.f schedule_ops.f \
	sim_initday.f sim_inityr.f simulate.f slrgen.f smeas.f snom.f \
	soil_chem.f soil_par.f soil_phys.f soil_write.f solp.f solt.f std1.f \
	std2.f std3.f stdaa.f storeinitial.f structure.f sub_subbasin.f \
	subaa.f subbasin.f subday.f submon.f substor.f subwq.f subyr.f \
	sumhyd.f sumv.f surface.f surfst_h2o.f surfstor.f surq_daycn.f \
	surq_greenampt.f swbl.f sweep.f swu.f tair.f tgen.f theta.f \
	tillfactor.f tillmix.f tmeas.f tran.f transfer.f tstr.f ttcoef.f \
	urb_bmp.f urban.f urbanhr.f varinit.f vbl.f virtual.f volq.f washp.f \
	watbal.f water_hru.f watqual.f watqual2.f wattable.f watuse.f \
	weatgn.f wetlan.f wmeas.f wndgen.f writea.f writeaa.f writed.f \
	writem.f xmon.f ysed.f zero0.f zero1.f zero2a.f zero2b.f zero_urbn.f zeroini.f

OBJS =	addh.o albedo.o allocate_parms.o alph.o anfert.o apex_day.o apply.o \
	ascrv.o atri.o aunif.o autoirr.o aveval.o bacteria.o biozone.o \
	bmp_det_pond.o bmp_ri_pond.o bmp_sand_filter.o bmp_sed_pond.o \
	bmp_wet_pond.o bmpfixed.o bmpinit.o buffer.o burnop.o canopyint.o \
	caps.o carbon_new.o chkcst.o clgen.o clicon.o command.o conapply.o \
	confert.o crackflow.o crackvol.o curno.o dailycn.o decay.o depstor.o \
	distrib_bmps.o dormant.o drains.o dstn1.o ee.o eiusle.o enrsb.o \
	erfc.o estimate_ksat.o etact.o etpot.o expo.o fert.o filter.o filtw.o \
	finalbal.o gcycl.o getallo.o getallo2.o grass_wway.o graze.o grow.o \
	gw_no3.o gwmod.o gwnutr.o h2omgt_init.o harvestop.o harvgrainop.o \
	harvkillop.o header.o headout.o hhnoqual.o hhwatqual.o hmeas.o \
	hruaa.o hruallo.o hruday.o hrumon.o hrupond.o hrupondhr.o hruyr.o \
	hydroinit.o impnd_init.o impndaa.o impndday.o impndmon.o impndyr.o \
	irr_rch.o irr_res.o irrigate.o irrsub.o jdt.o killop.o lakeq.o \
	latsed.o layersplit.o lwqdef.o main.o modparm.o ndenit.o newtillmix.o \
	nfix.o nitvol.o nlch.o nminrl.o noqual.o npup.o nrain.o nup.o nuts.o \
	openwth.o operatn.o orgn.o orgncswat.o origtile.o ovr_sed.o \
	percmacro.o percmain.o percmicro.o pestlch.o pestw.o pesty.o pgen.o \
	pgenhr.o pkq.o plantmod.o plantop.o pmeas.o pminrl.o pminrl2.o pond.o \
	pondhr.o pothole.o potholehr.o print_hyd.o psed.o qman.o ran1.o \
	rchaa.o rchday.o rchinit.o rchmon.o rchuse.o rchyr.o reachout.o \
	readatmodep.o readbsn.o readchm.o readcnst.o readfcst.o readfert.o \
	readfig.o readfile.o readgw.o readhru.o readinpt.o readlup.o \
	readlwq.o readmgt.o readmon.o readops.o readparmfile.o readpest.o \
	readplant.o readpnd.o readres.o readrte.o readru.o readsdr.o \
	readsepticbz.o readseptwq.o readsno.o readsol.o readsub.o readswq.o \
	readtill.o readurban.o readwgn.o readwus.o readwwq.o readyr.o \
	reccnst.o recday.o rechour.o recmon.o recyear.o regres.o res.o \
	resbact.o resetlu.o reshr.o resinit.o resnut.o rewind_init.o rhgen.o \
	rootfr.o route.o routels.o routeunit.o routres.o rsedaa.o rseday.o \
	rsedmon.o rsedyr.o rtbact.o rtday.o rteinit.o rthmusk.o rthpest.o \
	rthr.o rthsed.o rtmusk.o rtout.o rtover.o rtpest.o rtsed.o \
	rtsed_bagnold.o rtsed_kodatie.o rtsed_Molinas_Wu.o rtsed_yangsand.o \
	ruallo.o sat_excess.o save.o saveconc.o sched_mgt.o schedule_ops.o \
	sim_initday.o sim_inityr.o simulate.o slrgen.o smeas.o snom.o \
	soil_chem.o soil_par.o soil_phys.o soil_write.o solp.o solt.o std1.o \
	std2.o std3.o stdaa.o storeinitial.o structure.o sub_subbasin.o \
	subaa.o subbasin.o subday.o submon.o substor.o subwq.o subyr.o \
	sumhyd.o sumv.o surface.o surfst_h2o.o surfstor.o surq_daycn.o \
	surq_greenampt.o swbl.o sweep.o swu.o tair.o tgen.o theta.o \
	tillfactor.o tillmix.o tmeas.o tran.o transfer.o tstr.o ttcoef.o \
	urb_bmp.o urban.o urbanhr.o varinit.o vbl.o virtual.o volq.o washp.o \
	watbal.o water_hru.o watqual.o watqual2.o wattable.o watuse.o \
	weatgn.o wetlan.o wmeas.o wndgen.o writea.o writeaa.o writed.o \
	writem.o xmon.o ysed.o zero0.o zero1.o zero2a.o zero2b.o zero_urbn.o zeroini.o

all: $(PROG)

$(PROG): $(OBJS)
	$(FC) $(FCFLAGS) -o $@ $(OBJS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f

.f.o:
	$(FC) $(FCFLAGS) -c $<

addh.o:modparm.o
albedo.o:modparm.o
allocate_parms.o:modparm.o
alph.o:modparm.o
anfert.o:modparm.o
apex_day.o:modparm.o
apply.o:modparm.o
ascrv.o:modparm.o
atri.o:modparm.o
aunif.o:modparm.o
autoirr.o:modparm.o
aveval.o:modparm.o
bacteria.o:modparm.o
biozone.o:modparm.o
bmp_det_pond.o:modparm.o
bmp_ri_pond.o:modparm.o
bmp_sand_filter.o:modparm.o
bmp_sed_pond.o:modparm.o
bmp_wet_pond.o:modparm.o
bmpfixed.o:modparm.o
bmpinit.o:modparm.o
buffer.o:modparm.o
burnop.o:modparm.o
canopyint.o:modparm.o
caps.o:modparm.o
carbon_new.o:modparm.o
chkcst.o:modparm.o
clgen.o:modparm.o
clicon.o:modparm.o
command.o:modparm.o
conapply.o:modparm.o
confert.o:modparm.o
crackflow.o:modparm.o
crackvol.o:modparm.o
curno.o:modparm.o
dailycn.o:modparm.o
decay.o:modparm.o
depstor.o:modparm.o
distrib_bmps.o:modparm.o
dormant.o:modparm.o
drains.o:modparm.o
dstn1.o:modparm.o
ee.o:modparm.o
eiusle.o:modparm.o
enrsb.o:modparm.o
erfc.o:modparm.o
estimate_ksat.o:modparm.o
etact.o:modparm.o
etpot.o:modparm.o
expo.o:modparm.o
fert.o:modparm.o
filter.o:modparm.o
filtw.o:modparm.o
finalbal.o:modparm.o
gcycl.o:modparm.o
getallo.o:modparm.o
getallo2.o:modparm.o
grass_wway.o:modparm.o
graze.o:modparm.o
grow.o:modparm.o
gw_no3.o:modparm.o
gwmod.o:modparm.o
gwnutr.o:modparm.o
h2omgt_init.o:modparm.o
harvestop.o:modparm.o
harvgrainop.o:modparm.o
harvkillop.o:modparm.o
header.o:modparm.o
headout.o:modparm.o
hhnoqual.o:modparm.o
hhwatqual.o:modparm.o
hmeas.o:modparm.o
hruaa.o:modparm.o
hruallo.o:modparm.o
hruday.o:modparm.o
hrumon.o:modparm.o
hrupond.o:modparm.o
hrupondhr.o:modparm.o
hruyr.o:modparm.o
hydroinit.o:modparm.o
impnd_init.o:modparm.o
impndaa.o:modparm.o
impndday.o:modparm.o
impndmon.o:modparm.o
impndyr.o:modparm.o
irr_rch.o:modparm.o
irr_res.o:modparm.o
irrigate.o:modparm.o
irrsub.o:modparm.o
jdt.o:modparm.o
killop.o:modparm.o
lakeq.o:modparm.o
latsed.o:modparm.o
layersplit.o:modparm.o
lwqdef.o:modparm.o
main.o:modparm.o
ndenit.o:modparm.o
newtillmix.o:modparm.o
nfix.o:modparm.o
nitvol.o:modparm.o
nlch.o:modparm.o
nminrl.o:modparm.o
noqual.o:modparm.o
npup.o:modparm.o
nrain.o:modparm.o
nup.o:modparm.o
nuts.o:modparm.o
openwth.o:modparm.o
operatn.o:modparm.o
orgn.o:modparm.o
orgncswat.o:modparm.o
origtile.o:modparm.o
ovr_sed.o:modparm.o
percmacro.o:modparm.o
percmain.o:modparm.o
percmicro.o:modparm.o
pestlch.o:modparm.o
pestw.o:modparm.o
pesty.o:modparm.o
pgen.o:modparm.o
pgenhr.o:modparm.o
pkq.o:modparm.o
plantmod.o:modparm.o
plantop.o:modparm.o
pmeas.o:modparm.o
pminrl.o:modparm.o
pminrl2.o:modparm.o
pond.o:modparm.o
pondhr.o:modparm.o
pothole.o:modparm.o
potholehr.o:modparm.o
print_hyd.o:modparm.o
psed.o:modparm.o
qman.o:modparm.o
ran1.o:modparm.o
rchaa.o:modparm.o
rchday.o:modparm.o
rchinit.o:modparm.o
rchmon.o:modparm.o
rchuse.o:modparm.o
rchyr.o:modparm.o
reachout.o:modparm.o
readatmodep.o:modparm.o
readbsn.o:modparm.o
readchm.o:modparm.o
readcnst.o:modparm.o
readfcst.o:modparm.o
readfert.o:modparm.o
readfig.o:modparm.o
readfile.o:modparm.o
readgw.o:modparm.o
readhru.o:modparm.o
readinpt.o:modparm.o
readlup.o:modparm.o
readlwq.o:modparm.o
readmgt.o:modparm.o
readmon.o:modparm.o
readops.o:modparm.o
readparmfile.o:modparm.o
readpest.o:modparm.o
readplant.o:modparm.o
readpnd.o:modparm.o
readres.o:modparm.o
readrte.o:modparm.o
readru.o:modparm.o
readsdr.o:modparm.o
readsepticbz.o:modparm.o
readseptwq.o:modparm.o
readsno.o:modparm.o
readsol.o:modparm.o
readsub.o:modparm.o
readswq.o:modparm.o
readtill.o:modparm.o
readurban.o:modparm.o
readwgn.o:modparm.o
readwus.o:modparm.o
readwwq.o:modparm.o
readyr.o:modparm.o
reccnst.o:modparm.o
recday.o:modparm.o
rechour.o:modparm.o
recmon.o:modparm.o
recyear.o:modparm.o
regres.o:modparm.o
res.o:modparm.o
resbact.o:modparm.o
resetlu.o:modparm.o
reshr.o:modparm.o
resinit.o:modparm.o
resnut.o:modparm.o
rewind_init.o:modparm.o
rhgen.o:modparm.o
rootfr.o:modparm.o
route.o:modparm.o
routels.o:modparm.o
routeunit.o:modparm.o
routres.o:modparm.o
rsedaa.o:modparm.o
rseday.o:modparm.o
rsedmon.o:modparm.o
rsedyr.o:modparm.o
rtbact.o:modparm.o
rtday.o:modparm.o
rteinit.o:modparm.o
rthmusk.o:modparm.o
rthpest.o:modparm.o
rthr.o:modparm.o
rthsed.o:modparm.o
rtmusk.o:modparm.o
rtout.o:modparm.o
rtover.o:modparm.o
rtpest.o:modparm.o
rtsed.o:modparm.o
rtsed_Molinas_Wu.o:modparm.o
rtsed_bagnold.o:modparm.o
rtsed_kodatie.o:modparm.o
rtsed_yangsand.o:modparm.o
ruallo.o:modparm.o
sat_excess.o:modparm.o
save.o:modparm.o
saveconc.o:modparm.o
sched_mgt.o:modparm.o
schedule_ops.o:modparm.o
sim_initday.o:modparm.o
sim_inityr.o:modparm.o
simulate.o:modparm.o
slrgen.o:modparm.o
smeas.o:modparm.o
snom.o:modparm.o
soil_chem.o:modparm.o
soil_par.o:modparm.o
soil_phys.o:modparm.o
soil_write.o:modparm.o
solp.o:modparm.o
solt.o:modparm.o
std1.o:modparm.o
std2.o:modparm.o
std3.o:modparm.o
stdaa.o:modparm.o
storeinitial.o:modparm.o
structure.o:modparm.o
sub_subbasin.o:modparm.o
subaa.o:modparm.o
subbasin.o:modparm.o
subday.o:modparm.o
submon.o:modparm.o
substor.o:modparm.o
subwq.o:modparm.o
subyr.o:modparm.o
sumhyd.o:modparm.o
sumv.o:modparm.o
surface.o:modparm.o
surfst_h2o.o:modparm.o
surfstor.o:modparm.o
surq_daycn.o:modparm.o
surq_greenampt.o:modparm.o
swbl.o:modparm.o
sweep.o:modparm.o
swu.o:modparm.o
tair.o:modparm.o
tgen.o:modparm.o
theta.o:modparm.o
tillfactor.o:modparm.o
tillmix.o:modparm.o
tmeas.o:modparm.o
tran.o:modparm.o
transfer.o:modparm.o
tstr.o:modparm.o
ttcoef.o:modparm.o
urb_bmp.o:modparm.o
urban.o:modparm.o
urbanhr.o:modparm.o
varinit.o:modparm.o
vbl.o:modparm.o
virtual.o:modparm.o
volq.o:modparm.o
washp.o:modparm.o
watbal.o:modparm.o
water_hru.o:modparm.o
watqual.o:modparm.o
watqual2.o:modparm.o
wattable.o:modparm.o
watuse.o:modparm.o
weatgn.o:modparm.o
wetlan.o:modparm.o
wmeas.o:modparm.o
wndgen.o:modparm.o
writea.o:modparm.o
writeaa.o:modparm.o
writed.o:modparm.o
writem.o:modparm.o
xmon.o:modparm.o
ysed.o:modparm.o
zero0.o:modparm.o
zero1.o:modparm.o
zero2a.o:modparm.o
zero2b.o:modparm.o
zero_urbn.o:modparm.o
zeroini.o:modparm.o

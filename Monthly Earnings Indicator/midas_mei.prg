cd "C:\Users\aelde\OneDrive\Documents\GitHub\Reports\Monthly Earnings Indicator"

wfcreate(wf=COE_NOWCAST) m 2022:7 2025:6

import "C:\Users\aelde\OneDrive\Documents\GitHub\Reports\Monthly Earnings Indicator\MEI.csv" ftype=ascii rectype=crlf skip=0 fieldtype=delimited delim=comma colhead=1 eoltype=pad badfield=NA @freq M @id @date(date) @destid @date @smpl @all

pagerename Untitled Monthly

series mkts = EGW+WT+RT+AFS+TPW+IMT+FIS+RHR+PST+ASS+ARS+OTS
series nmkts =  PAS+ET+HCS



pagecreate quarterly 1959:3 2026:4
pagerename Untitled quarterly

import "C:\Users\aelde\OneDrive\Documents\GitHub\Reports\Monthly Earnings Indicator\IND_HRS.csv" ftype=ascii rectype=crlf skip=0 fieldtype=delimited delim=comma colhead=1 eoltype=pad badfield=NA @freq M @id @date(date) @destid @date @smpl @all

import "C:\Users\aelde\OneDrive\Documents\GitHub\Reports\Monthly Earnings Indicator\Hours_LFS.csv" ftype=ascii rectype=crlf skip=0 fieldtype=delimited delim=comma colhead=1 eoltype=pad badfield=NA @freq M @id @date(date) @destid @date @smpl @all

import C:\\Users\\aelde\\OneDrive\\Documents\\GitHub\\MACRO_DATA\\MACRO_DATA\\md2025-07-07.csv ftype=ascii rectype=crlf skip=0 fieldtype=delimited na="NA" delim=comma colhead=1 eoltype=pad badfield=NA @id @date(date) @smpl @all



smpl @all


series coe_mkts = coe_EGW+coe_WT+coe_RT+coe_AFS+coe_TPW+coe_IMT+coe_FIS+coe_RHR+coe_PST+coe_ASS+coe_ARS+coe_OTS
series coe_nmkts =  coe_PAS+coe_ET+coe_HCS


%h = @wflookup("COE_*","series")
for %v {%h}
	series pc_{%v} = @pc({%v})
next

	series pc_coe = @pc(coe)

'-------------------------------------------------------------------------
' AGGREGATE
'-------------------------------------------------------------------------

smpl 2022:3 2025:1
for %type step almon 

equation _coe_ncst_{%type}.midas(maxlag=8, lag=auto, midwgt={%type}) pc_coe c @ monthly\@pc(all)

next

equation _coe_ncst_beta.midas(fixedlag=8, midwgt=beta) pc_coe c @ monthly\@pc(all)

pc_coe.fcastavg(forcsmpl="2025:2 2025:2") _coe_ncst_step _coe_ncst_almon _coe_ncst_beta

smpl @all

series coe_ncst = coe(-1)*(1+pc_coe_f/100)

series waena_ncst = coe_ncst/(hw_lfs)


'equation _waena_ncst.ls dlog(waena) = c(1) +c(2)*dlog(waena_ncst)
'smpl 2025:1 2025:1
'_waena_ncst.forecast(e, g) waenaf

'-------------------------------------------------------------------------
' Industry
'-------------------------------------------------------------------------
series ots_hw = os_hw
series  MKTS_HW =EGW_HW+WT_HW+RT_HW+AFS_HW+TPW_HW+IMT_HW+FIS_HW+RHR_HW+PST_HW+ASS_HW+ARS_HW+OS_HW
series NMKTS_HW=PAS_HW+ET_HW+HCS_HW


for %i AG	MIN	MAN	EGW	CONS	WT	RT	AFS	TPW	IMT	FIS	RHR	PST	ASS	ET	HCS	ARS	OTS	PAS mkts nmkts
smpl 2022:3 2025:1



for %type step almon 

equation _coe_{%i}_ncst_{%type}.midas(maxlag=8, lag=auto, midwgt={%type}) pc_coe_{%i} c  @ monthly\@pc({%i})

next


equation _coe_{%i}_ncst_beta.midas(fixedlag=8, midwgt=beta) pc_coe_{%i} c  @ monthly\@pc({%i})

pc_coe_{%i}.fcastavg(forcsmpl="2025:2 2025:2") _coe_{%i}_ncst_step _coe_{%i}_ncst_almon _coe_{%i}_ncst_beta

smpl @all

series coe_{%i}_ncst = coe_{%i}(-1)*(1+pc_coe_{%i}_f/100)


next

'series coe_mkts_ncst = coe_EGW_ncst+coe_WT_ncst+coe_RT_ncst+coe_AFS_ncst+coe_TPW_ncst+coe_IMT_ncst+coe_FIS_ncst+coe_RHR_ncst+coe_PST_ncst+coe_ASS_ncst+coe_ARS_ncst+coe_OTS_ncst

group g_coe_ncst coe_*

wfsave(type=text) "C:\Users\aelde\OneDrive\Documents\GitHub\Reports\Monthly Earnings Indicator\coe_nowcast.csv"   @keep g_coe_ncst



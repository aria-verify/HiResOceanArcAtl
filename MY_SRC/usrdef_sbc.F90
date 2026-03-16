MODULE usrdef_sbc
   !!======================================================================
   !!                     ***  MODULE  usrdef_sbc  ***
   !!
   !!                     ===  GYRE configuration  ===
   !!
   !! User defined :   surface forcing of a user configuration
   !!======================================================================
   !! History :  4.0   ! 2016-03  (S. Flavoni, G. Madec)  user defined interface
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usrdef_sbc    : user defined surface bounday conditions in GYRE case
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE sbc_ice        ! Surface boundary condition: ocean fields
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! distribued memory computing library
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_fortran    !

   IMPLICIT NONE
   PRIVATE

   PUBLIC   usrdef_sbc_oce       ! routine called in sbcmod module
   PUBLIC   usrdef_sbc_ice_tau   ! routine called by icestp.F90 for ice dynamics
   PUBLIC   usrdef_sbc_ice_flx   ! routine called by icestp.F90 for ice thermo

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "single_precision_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: usrdef_sbc.F90 15145 2021-07-26 16:16:45Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE usrdef_sbc_oce( kt, Kbb )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE usrdef_sbc  ***
      !!              
      !! ** Purpose :   provide at each time-step the GYRE surface boundary
      !!              condition, i.e. the momentum, heat and freshwater fluxes.
      !!
      !! ** Method  :   analytical seasonal cycle for GYRE configuration.
      !!                CAUTION : never mask the surface stress field !
      !!
      !! ** Action  : - set the ocean surface boundary condition, i.e.   
      !!                   utau, vtau, taum, wndm, qns, qsr, emp, sfx
      !!
      !! Reference : Hazeleger, W., and S. Drijfhout, JPO, 30, 677-695, 2000.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      INTEGER, INTENT(in) ::   Kbb  ! ocean time index
      !!
      INTEGER  ::   ji, jj                 ! dummy loop indices
      INTEGER  ::   zyear0                 ! initial year 
      INTEGER  ::   zmonth0                ! initial month
      INTEGER  ::   zday0                  ! initial day
      INTEGER  ::   zday_year0             ! initial day since january 1st
      REAL(wp) ::   ztau     , ztau_sais   ! wind intensity and of the seasonal cycle
      REAL(wp) ::   ztime                  ! time in hour
      REAL(wp) ::   ztimemax , ztimemin    ! 21th June, and 21th decem. if date0 = 1st january
      REAL(wp) ::   ztimemax1, ztimemin1   ! 21th June, and 21th decem. if date0 = 1st january
      REAL(wp) ::   ztimemax2, ztimemin2   ! 21th June, and 21th decem. if date0 = 1st january
      REAL(wp) ::   ztaun                  ! intensity
      REAL(wp) ::   zemp_S, zemp_N, zemp_sais, zTstar
      REAL(wp) ::   zcos_sais1, zcos_sais2, ztrp, zconv, t_star
      REAL(wp) ::   zsumemp, zsurf
      REAL(wp) ::   zrhoa  = 1.22         ! Air density kg/m3
      REAL(wp) ::   zcdrag = 1.5e-3       ! drag coefficient
      REAL(wp) ::   ztx, zty, zmod, zcoef ! temporary variables
      REAL(wp) ::   zyydd                 ! number of days in one year
      !!---------------------------------------------------------------------
     !DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )   ! emp and rnf used in sshwzv over the whole domain
         qsr(:,:) = 0.0_wp
         qns(:,:) = 0.0_wp
         emp(:,:) = 0.0_wp
         sfx(:,:) = 0.0_wp                                         ! no salt flux
        ! END_2D


       !DO_2D( 1, 1, 1, 1 )
        utau(:,:) = 0.0_wp
        vtau(:,:) = 0.0_wp
       !END_2D

      !DO_2D( 0, 0, 0, 0 )
         taum(:,:) = 0.0_wp
         wndm(:,:) = 0.0_wp
        !END_2D
      !RDP below is needed to stop patches appearing when ice conc =0...
      qns_tot(:,:) = 0.0_wp
      qsr_tot(:,:) = 0.0_wp

      !CALL lbc_lnk( 'usrdef_sbc_oce', emp, 'T', 1.0_wp )
      !CALL lbc_lnk( 'usrdef_sbc_oce', taum, 'T', 1.0_wp )
      !CALL lbc_lnk( 'usrdef_sbc_oce', wndm, 'T', 1.0_wp )
      !CALL lbc_lnk( 'usrdef_sbc_oce', qsr, 'T', 1.0_wp )
      !CALL lbc_lnk( 'usrdef_sbc_oce', qns, 'T', 1.0_wp )
      !CALL lbc_lnk( 'usrdef_sbc_oce', sfx, 'T', 1.0_wp )

      !
   END SUBROUTINE usrdef_sbc_oce


   SUBROUTINE usrdef_sbc_ice_tau( kt )
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !!utau_ice(:,:) = 0.0_wp       !: atmos-ice u-stress. VP: I-pt ; EVP: U,V-pts   [N/m2]
      !!vtau_ice(:,:) = 0.0_wp       !: atmos-ice v-stress. VP: I-pt ; EVP: U,V-pts   [N/m2]
   END SUBROUTINE usrdef_sbc_ice_tau


   SUBROUTINE usrdef_sbc_ice_flx( kt, phs, phi )
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::   phs    ! snow thickness
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::   phi    ! ice thickness
      sprecip(:,:) = 0.0_wp

      !!qns_ice(:,:,:) = 0.0_wp        !: non solar heat flux over ice                  [W/m2]
      !!qsr_ice(:,:,:) = 0.0_wp        !: solar heat flux over ice                      [W/m2]
      !!qla_ice(:,:,:) = 0.0_wp        !: latent flux over ice                          [W/m2]
      !!dqla_ice(:,:,:) = 0.0_wp       !: latent sensibility over ice                 [W/m2/K]
      !!dqns_ice(:,:,:) = 0.0_wp       !: non solar heat flux over ice (LW+SEN+LA)    [W/m2/K]
      !!!tn_ice(:,:,:) = 0.0_wp         !: ice surface temperature                          [K]
      !!!alb_ice(:,:,:) = 0.0_wp        !: ice albedo                                       [-]

      !!qml_ice(:,:,:) = 0.0_wp        !: heat available for snow / ice surface melting     [W/m2]
      !!qcn_ice(:,:,:) = 0.0_wp        !: heat conduction flux in the layer below surface   [W/m2]
      !!qtr_ice_top(:,:,:) = 0.0_wp    !: solar flux transmitted below the ice surface      [W/m2]

      !!emp_ice(:,:) = 0.0_wp        !: sublimation - precip over sea ice          [kg/m2/s]

      !!!topmelt(:,:,:) = 0.0_wp            !: category topmelt
      !!!botmelt(:,:,:) = 0.0_wp            !: category botmelt

      !!evap_ice(:,:,:) = 0.0_wp       !: sublimation                              [kg/m2/s]
      !!devap_ice(:,:,:) = 0.0_wp      !: sublimation sensitivity                [kg/m2/s/K]
      !!qns_oce(:,:) = 0.0_wp        !: non solar heat flux over ocean              [W/m2]
      !!qsr_oce(:,:) = 0.0_wp        !: non solar heat flux over ocean              [W/m2]
      !!qemp_oce(:,:) = 0.0_wp       !: heat flux of precip and evap over ocean     [W/m2]
      !!qemp_ice(:,:) = 0.0_wp       !: heat flux of precip and evap over ice       [W/m2]
      !!qevap_ice(:,:,:) = 0.0_wp      !: heat flux of evap over ice                  [W/m2]
      !!qprec_ice(:,:) = 0.0_wp      !: enthalpy of precip over ice                 [J/m3]
      !!emp_oce(:,:) = 0.0_wp        !: evap - precip over ocean                 [kg/m2/s]
      !!wndm_ice(:,:) = 0.0_wp       !: wind speed module at T-point                 [m/s]
      !!!sstfrz(:,:) = 0.0_wp         !: sea surface freezing temperature            [degC]

   END SUBROUTINE usrdef_sbc_ice_flx

   !!======================================================================
END MODULE usrdef_sbc

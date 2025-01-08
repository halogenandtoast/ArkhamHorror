module Arkham.Asset.Assets.SmallRadio (smallRadio) where

import Arkham.Ability
import Arkham.Asset.Assets.AveryClaypoolAntarcticGuide
import Arkham.Asset.Assets.AveryClaypoolAntarcticGuideResolute
import Arkham.Asset.Assets.DanforthBrilliantStudent
import Arkham.Asset.Assets.DanforthBrilliantStudentResolute
import Arkham.Asset.Assets.DrAmyKenslerProfessorOfBiology
import Arkham.Asset.Assets.DrAmyKenslerProfessorOfBiologyResolute
import Arkham.Asset.Assets.DrMalaSinhaDaringPhysician
import Arkham.Asset.Assets.DrMalaSinhaDaringPhysicianResolute
import Arkham.Asset.Assets.EliyahAshevakDogHandler
import Arkham.Asset.Assets.EliyahAshevakDogHandlerResolute
import Arkham.Asset.Assets.JamesCookieFredericksDubiousChoice
import Arkham.Asset.Assets.JamesCookieFredericksDubiousChoiceResolute
import Arkham.Asset.Assets.ProfessorWilliamDyerProfessorOfGeology
import Arkham.Asset.Assets.ProfessorWilliamDyerProfessorOfGeologyResolute
import Arkham.Asset.Assets.RoaldEllsworthIntrepidExplorer
import Arkham.Asset.Assets.RoaldEllsworthIntrepidExplorerResolute
import Arkham.Asset.Assets.TakadaHirokoAeroplaneMechanic
import Arkham.Asset.Assets.TakadaHirokoAeroplaneMechanicResolute
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.CampaignLog
import Arkham.Card
import Arkham.GameT
import Arkham.Name
import Control.Monad.Trans.Class

newtype SmallRadio = SmallRadio AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smallRadio :: AssetCard SmallRadio
smallRadio = asset SmallRadio Cards.smallRadio

instance HasAbilities SmallRadio where
  getAbilities (SmallRadio a) =
    map adjustCost
      $ whenPartner
        averyClaypoolAntarcticGuide
        Safe
        (getAbilities (cbCardBuilder averyClaypoolAntarcticGuide a.cardId (a.id, a.owner)))
      <> whenPartner
        averyClaypoolAntarcticGuide
        Resolute
        (getAbilities (cbCardBuilder averyClaypoolAntarcticGuideResolute a.cardId (a.id, a.owner)))
      <> whenPartner
        danforthBrilliantStudent
        Safe
        (getAbilities (cbCardBuilder danforthBrilliantStudent a.cardId (a.id, a.owner)))
      <> whenPartner
        danforthBrilliantStudent
        Resolute
        (getAbilities (cbCardBuilder danforthBrilliantStudentResolute a.cardId (a.id, a.owner)))
      <> whenPartner
        drAmyKenslerProfessorOfBiology
        Safe
        (getAbilities (cbCardBuilder drAmyKenslerProfessorOfBiology a.cardId (a.id, a.owner)))
      <> whenPartner
        drAmyKenslerProfessorOfBiology
        Resolute
        (getAbilities (cbCardBuilder drAmyKenslerProfessorOfBiologyResolute a.cardId (a.id, a.owner)))
      <> whenPartner
        drMalaSinhaDaringPhysician
        Safe
        (getAbilities (cbCardBuilder drMalaSinhaDaringPhysician a.cardId (a.id, a.owner)))
      <> whenPartner
        drMalaSinhaDaringPhysician
        Resolute
        (getAbilities (cbCardBuilder drMalaSinhaDaringPhysicianResolute a.cardId (a.id, a.owner)))
      <> whenPartner
        eliyahAshevakDogHandler
        Safe
        (getAbilities (cbCardBuilder eliyahAshevakDogHandler a.cardId (a.id, a.owner)))
      <> whenPartner
        eliyahAshevakDogHandler
        Resolute
        (getAbilities (cbCardBuilder eliyahAshevakDogHandlerResolute a.cardId (a.id, a.owner)))
      <> whenPartner
        jamesCookieFredericksDubiousChoice
        Safe
        (getAbilities (cbCardBuilder jamesCookieFredericksDubiousChoice a.cardId (a.id, a.owner)))
      <> whenPartner
        jamesCookieFredericksDubiousChoice
        Resolute
        (getAbilities (cbCardBuilder jamesCookieFredericksDubiousChoiceResolute a.cardId (a.id, a.owner)))
      <> whenPartner
        professorWilliamDyerProfessorOfGeology
        Safe
        (getAbilities (cbCardBuilder professorWilliamDyerProfessorOfGeology a.cardId (a.id, a.owner)))
      <> whenPartner
        professorWilliamDyerProfessorOfGeology
        Resolute
        ( getAbilities (cbCardBuilder professorWilliamDyerProfessorOfGeologyResolute a.cardId (a.id, a.owner))
        )
      <> whenPartner
        roaldEllsworthIntrepidExplorer
        Safe
        (getAbilities (cbCardBuilder roaldEllsworthIntrepidExplorer a.cardId (a.id, a.owner)))
      <> whenPartner
        roaldEllsworthIntrepidExplorer
        Resolute
        (getAbilities (cbCardBuilder roaldEllsworthIntrepidExplorerResolute a.cardId (a.id, a.owner)))
      <> whenPartner
        takadaHirokoAeroplaneMechanic
        Safe
        (getAbilities (cbCardBuilder takadaHirokoAeroplaneMechanic a.cardId (a.id, a.owner)))
      <> whenPartner
        takadaHirokoAeroplaneMechanic
        Resolute
        (getAbilities (cbCardBuilder takadaHirokoAeroplaneMechanicResolute a.cardId (a.id, a.owner)))
   where
    adjustCost = overCost \c -> case totalActionCost c of
      0 -> exhaust a <> assetUseCost a Supply 1
      n -> ActionCost n <> exhaust a <> assetUseCost a Supply 1
    whenPartner :: forall a. AssetCard a -> PartnerStatus -> [Ability] -> [Ability]
    whenPartner p s =
      map
        ( \ab ->
            withTooltip (toTitle p.name)
              $ ab
                { abilityCriteria = ab.criteria <> PartnerHasStatus p.cardCode s
                , abilitySource = proxy (CardCodeSource p.cardCode) a
                }
        )

runPartner :: MonadTrans m => AssetAttrs -> CardCode -> Message -> m GameT ()
runPartner attrs cCode msg = do
  let
    go :: forall a m. (RunMessage a, MonadTrans m) => AssetCard a -> m GameT ()
    go x =
      let z = cbCardBuilder x attrs.cardId (attrs.id, attrs.owner)
       in void $ liftRunMessage msg z
  if
    | cCode == averyClaypoolAntarcticGuide.cardCode -> go averyClaypoolAntarcticGuide
    | cCode == averyClaypoolAntarcticGuideResolute.cardCode -> go averyClaypoolAntarcticGuideResolute
    | cCode == danforthBrilliantStudent.cardCode -> go danforthBrilliantStudent
    | cCode == danforthBrilliantStudentResolute.cardCode -> go danforthBrilliantStudentResolute
    | cCode == drAmyKenslerProfessorOfBiology.cardCode -> go drAmyKenslerProfessorOfBiology
    | cCode == drAmyKenslerProfessorOfBiologyResolute.cardCode ->
        go drAmyKenslerProfessorOfBiologyResolute
    | cCode == drMalaSinhaDaringPhysician.cardCode -> go drMalaSinhaDaringPhysician
    | cCode == drMalaSinhaDaringPhysicianResolute.cardCode -> go drMalaSinhaDaringPhysicianResolute
    | cCode == eliyahAshevakDogHandler.cardCode -> go eliyahAshevakDogHandler
    | cCode == eliyahAshevakDogHandlerResolute.cardCode -> go eliyahAshevakDogHandlerResolute
    | cCode == jamesCookieFredericksDubiousChoice.cardCode -> go jamesCookieFredericksDubiousChoice
    | cCode == jamesCookieFredericksDubiousChoiceResolute.cardCode ->
        go jamesCookieFredericksDubiousChoiceResolute
    | cCode == professorWilliamDyerProfessorOfGeology.cardCode ->
        go professorWilliamDyerProfessorOfGeology
    | cCode == professorWilliamDyerProfessorOfGeologyResolute.cardCode ->
        go professorWilliamDyerProfessorOfGeologyResolute
    | cCode == roaldEllsworthIntrepidExplorer.cardCode -> go roaldEllsworthIntrepidExplorer
    | cCode == roaldEllsworthIntrepidExplorerResolute.cardCode ->
        go roaldEllsworthIntrepidExplorerResolute
    | cCode == takadaHirokoAeroplaneMechanic.cardCode -> go takadaHirokoAeroplaneMechanic
    | cCode == takadaHirokoAeroplaneMechanicResolute.cardCode ->
        go takadaHirokoAeroplaneMechanicResolute
    | otherwise -> error "invalid partner code"

instance RunMessage SmallRadio where
  runMessage msg a@(SmallRadio attrs) = runQueueT $ case msg of
    UseCardAbility iid (ProxySource (CardCodeSource cCode) (isSource attrs -> True)) n ws ps -> do
      runPartner attrs cCode (UseCardAbility iid (toSource attrs) n ws ps)
      pure $ overAttrs (setMeta cCode) a
    ResolvedAbility (abilitySource -> ProxySource (CardCodeSource _) (isSource attrs -> True)) -> do
      pure $ overAttrs (setMeta Null) a
    _ -> case maybeResult attrs.meta of
      Nothing -> SmallRadio <$> liftRunMessage msg attrs
      Just ccode -> do
        _ <- runPartner attrs ccode msg
        pure a

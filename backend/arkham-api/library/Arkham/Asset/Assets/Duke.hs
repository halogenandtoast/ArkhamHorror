module Arkham.Asset.Assets.Duke (Duke (..), duke) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Game.Helpers
import Arkham.Helpers.Investigator
import Arkham.Helpers.SkillTest
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Window (defaultWindows)

newtype Duke = Duke AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duke :: AssetCard Duke
duke = allyWith Duke Cards.duke (2, 3) noSlots

instance HasModifiersFor Duke where
  getModifiersFor (Duke a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      guardM $ isSource a <$> MaybeT getSkillTestSource
      MaybeT getSkillTestAction >>= \case
        Action.Fight -> pure [BaseSkillOf #combat 4, DamageDealt 1]
        Action.Investigate -> pure [BaseSkillOf #intellect 4]
        _ -> pure []

instance HasAbilities Duke where
  getAbilities (Duke a) =
    [ fightAbility a 1 (exhaust a) ControlsThis
    , delayAdditionalCostsWhen (youExist $ InvestigatorCanMoveTo (a.ability 2) Anywhere)
        $ investigateAbility a 2 (exhaust a) ControlsThis
    ]

instance RunMessage Duke where
  runMessage msg a@(Duke attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      as <- select $ performableAbilityWithoutActionBy iid $ at_ (be iid) <> #basic <> #investigate
      let convert ab = noAOO $ decrease_ (ab {abilitySource = ProxySource ab.source source}) 1
      chooseOneM iid do
        for_ as \ab -> abilityLabeled iid (convert ab) nothing
        targetsM (getAccessibleLocations iid source) \lid' -> do
          moveTo attrs iid lid'
          doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      lid <- getJustLocation iid
      selectForMaybeM (BasicInvestigate lid) \ab ->
        whenM (getCanPerformAbility iid (defaultWindows iid) (decrease_ ab 1)) do
          sid <- getRandom
          investigate' <- mkInvestigateLocation sid iid attrs lid
          push $ CheckAdditionalActionCosts iid (toTarget lid) #investigate [toMessage investigate']
      pure a
    UseThisAbility iid (ProxySource (LocationSource lid) (isAbilitySource attrs 2 -> True)) _ -> do
      selectForMaybeM (BasicInvestigate lid) \ab ->
        whenM (getCanPerformAbility iid (defaultWindows iid) $ decrease_ ab 1) do
          sid <- getRandom
          pushM $ mkInvestigateLocation sid iid attrs lid
      pure a
    _ -> Duke <$> liftRunMessage msg attrs

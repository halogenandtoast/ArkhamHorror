module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.PlaguePolyp (plaguePolyp) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Placement (Placement (InPlayArea))

newtype PlaguePolyp = PlaguePolyp AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plaguePolyp :: AssetCard PlaguePolyp
plaguePolyp = asset PlaguePolyp Cards.plaguePolyp

healableHorrorInvestigator :: Sourceable source => source -> LocationMatcher -> InvestigatorMatcher
healableHorrorInvestigator source loc = HealableInvestigator (toSource source) #horror $ at_ loc

healableHorrorAlly :: Sourceable source => source -> LocationMatcher -> AssetMatcher
healableHorrorAlly source loc =
  HealableAsset (toSource source) #horror
    $ #ally
    <> at_ loc
    <> AssetControlledBy (affectsOthers Anyone)

instance HasModifiersFor PlaguePolyp where
  -- "Each investigator, Ally asset, and enemy at your location gets -1 health
  -- (to a minimum of 1)."
  getModifiersFor (PlaguePolyp a) = case a.placement of
    InPlayArea iid -> do
      let here = locationWithInvestigator iid
      modifySelect a (investigatorAt here) minusOneHealth
      modifySelect a (AssetAt here <> #ally) minusOneHealth
      modifySelect a (EnemyAt here) minusOneHealth
    _ -> pure ()
   where
    minusOneHealth = [HealthModifierWithMin (-1) (Min 1)]

instance HasAbilities PlaguePolyp where
  -- "[fast] During your turn, discard Plague Polyp: Heal 3 total horror from
  -- among investigators and Ally assets at your location."
  getAbilities (PlaguePolyp a) =
    [controlled a 1 (DuringTurn You <> canHeal) $ FastAbility (discardCost a)]
   where
    canHeal =
      oneOf
        [ exists $ healableHorrorInvestigator a YourLocation
        , exists $ healableHorrorAlly a YourLocation
        ]

instance RunMessage PlaguePolyp where
  runMessage msg a@(PlaguePolyp attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      doStep 3 msg
      pure a
    DoStep n (UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      let here = locationWithInvestigator iid
      investigators <- selectTargets $ healableHorrorInvestigator (attrs.ability 1) here
      allies <- selectTargets $ healableHorrorAlly (attrs.ability 1) here
      let choices = investigators <> allies
      unless (null choices) do
        chooseOneM iid $ targets choices \target -> healHorror target (attrs.ability 1) 1
        doNextStep msg
      pure a
    _ -> PlaguePolyp <$> liftRunMessage msg attrs

module Arkham.Investigator.Cards.HankSamson (hankSamson, HankSamson (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Classes.HasQueue (replaceMessageMatching)
import Arkham.Game.Helpers (onSameLocation)
import Arkham.Helpers.Investigator
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified, modified)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher

newtype HankSamson = HankSamson InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

hankSamson :: InvestigatorCard HankSamson
hankSamson =
  investigator HankSamson Cards.hankSamson
    $ Stats {health = 5, sanity = 5, willpower = 3, intellect = 1, combat = 5, agility = 3}

instance HasModifiersFor HankSamson where
  getModifiersFor (InvestigatorTarget iid) (HankSamson a) | iid /= a.id = do
    maybeModified a do
      liftGuardM $ onSameLocation iid a.placement
      pure [CanAssignDamageToInvestigator (toId a), CanAssignHorrorToInvestigator (toId a)]
  getModifiersFor target (HankSamson a) | isTarget a target = do
    if investigatorArt a == "10015" then pure [] else modified a [CannotHealHorror, CannotHealDamage]
  getModifiersFor _ _ = pure []

instance HasAbilities HankSamson where
  getAbilities (HankSamson a) = case investigatorArt a of
    "10015" -> [restrictedAbility a 1 Self $ freeReaction $ InvestigatorWouldBeDefeated #when ByAny You]
    "10016a" ->
      [ restrictedAbility a 1 (Self <> can.draw.cards You)
          $ freeReaction
          $ PlacedCounter #when You AnySource HorrorCounter
          $ atLeast 1
      ]
    "10016b" ->
      [ restrictedAbility a 1 (Self <> can.gain.resources You)
          $ freeReaction
          $ PlacedCounter #when You AnySource DamageCounter
          $ atLeast 1
      ]
    _ -> error "Impossible"

instance HasChaosTokenValue HankSamson where
  getChaosTokenValue iid ElderSign (HankSamson attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage HankSamson where
  runMessage msg i@(HankSamson attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 | investigatorArt attrs == "10015" -> do
      lift do
        replaceMessageMatching
          (\case InvestigatorWhenDefeated _ iid' -> iid == iid'; _ -> False)
          \case
            InvestigatorWhenDefeated source' _ -> [Msg.checkDefeated source' iid]
            _ -> error "invalid match"
        Msg.removeAllMessagesMatching \case
          AssignDamage (InvestigatorTarget iid') -> iid == iid'
          AssignedDamage (InvestigatorTarget iid') -> iid == iid'
          _ -> False
      pushWhenM (canHaveHorrorHealed attrs iid) $ HealHorror (toTarget iid) (toSource attrs) 5
      pushWhenM (canHaveDamageHealed attrs iid) $ HealDamage (toTarget iid) (toSource attrs) 5
      chooseOne iid [CardLabel "10016a" [DoStep 1 msg], CardLabel "10016b" [DoStep 2 msg]]
      pure
        . HankSamson
        $ attrs {investigatorAssignedHealthDamage = 0, investigatorAssignedSanityDamage = 0}
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      pure
        . HankSamson
        $ attrs
          { investigatorArt = "10016a"
          , investigatorHealth = 4
          , investigatorSanity = 6
          , investigatorIntellect = 3
          , investigatorCombat = 4
          , investigatorAgility = 4
          }
    DoStep 2 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      pure
        . HankSamson
        $ attrs
          { investigatorArt = "10016b"
          , investigatorHealth = 6
          , investigatorSanity = 4
          , investigatorWillpower = 4
          , investigatorCombat = 6
          }
    UseThisAbility iid (isSource attrs -> True) 1 | investigatorArt attrs == "10016a" -> do
      drawCardsIfCan iid attrs 1
      pure i
    UseThisAbility iid (isSource attrs -> True) 1 | investigatorArt attrs == "10016b" -> do
      gainResourcesIfCan iid attrs 2
      pure i
    ElderSignEffect iid | iid == attrs.id -> do
      case investigatorArt attrs of
        "10016a" -> do
          canHorrorAssets <- select $ assetControlledBy iid <> AssetWithAnyRemainingSanity
          unless (null canHorrorAssets) do
            chooseOne
              iid
              [ targetLabel asset [MoveTokens (toSource attrs) (toSource iid) (toTarget asset) #horror 1]
              | asset <- canHorrorAssets
              ]
        "10016b" -> do
          canDamageAssets <- select $ assetControlledBy iid <> AssetWithAnyRemainingHealth
          unless (null canDamageAssets) do
            chooseOne
              iid
              [ targetLabel asset [MoveTokens (toSource attrs) (toSource iid) (toTarget asset) #damage 1]
              | asset <- canDamageAssets
              ]
        _ -> pure ()
      pure i
    _ -> HankSamson <$> liftRunMessage msg attrs

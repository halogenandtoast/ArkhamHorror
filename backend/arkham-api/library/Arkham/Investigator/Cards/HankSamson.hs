module Arkham.Investigator.Cards.HankSamson (
  hankSamson,
  hankSamsonResoluteAssistant,
  hankSamsonResoluteWarden,
) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card.CardCode (CardCodeExact (..))
import Arkham.Classes.HasQueue (replaceMessageMatching)
import Arkham.Helpers.Investigator
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelfWhen)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (InvestigatorForm (..))
import Arkham.Matcher

newtype HankSamson = HankSamson InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

hankSamson :: InvestigatorCard HankSamson
hankSamson =
  investigator HankSamson Cards.hankSamson
    $ Stats {health = 5, sanity = 5, willpower = 3, intellect = 1, combat = 5, agility = 3}

hankSamsonResoluteAssistant :: InvestigatorCard HankSamson
hankSamsonResoluteAssistant =
  investigator HankSamson Cards.hankSamsonResoluteAssistant
    $ Stats {health = 4, sanity = 6, willpower = 3, intellect = 3, combat = 4, agility = 4}

hankSamsonResoluteWarden :: InvestigatorCard HankSamson
hankSamsonResoluteWarden =
  investigator HankSamson Cards.hankSamsonResoluteWarden
    $ Stats {health = 6, sanity = 4, willpower = 4, intellect = 1, combat = 6, agility = 3}

-- Which side of Hank's card is showing. Real Hank tracks this via
-- investigatorArt; a host transfigured into Hank (Transfiguration (2))
-- tracks it via the form's card code.
hankSide :: InvestigatorAttrs -> CardCodeExact
hankSide a = case investigatorForm a of
  TransfiguredForm c -> CardCodeExact c
  _ -> investigatorArt a

instance HasModifiersFor HankSamson where
  getModifiersFor (HankSamson a) = do
    modifySelect
      a
      (not_ (InvestigatorWithId a.id) <> colocatedWith a.id)
      [CanAssignDamageToInvestigator a.id, CanAssignHorrorToInvestigator a.id]
    modifySelect
      a
      (AssetAt (locationWithInvestigator a.id) <> #ally)
      [CanAssignDamageToInvestigator (toId a), CanAssignHorrorToInvestigator (toId a)]
    modifySelfWhen
      a
      (hankSide a /= "10015")
      [CannotHaveHorrorHealed, CannotHaveDamageHealed]

instance HasAbilities HankSamson where
  getAbilities (HankSamson a) = case hankSide a of
    "10016a" ->
      [ restricted a 1 (Self <> can.draw.cards You)
          $ freeReaction
          $ PlacedCounter #when You AnySource HorrorCounter (atLeast 1)
      ]
    "10016b" ->
      [ restricted a 1 (Self <> can.gain.resources You)
          $ freeReaction
          $ PlacedCounter #when You AnySource DamageCounter (atLeast 1)
      ]
    _ -> [restrictedAbility a 1 Self $ freeReaction $ InvestigatorWouldBeDefeated #when ByAny You]

instance HasChaosTokenValue HankSamson where
  getChaosTokenValue iid ElderSign (HankSamson attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage HankSamson where
  runMessage msg i@(HankSamson attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 | hankSide attrs == "10015" -> do
      lift do
        replaceMessageMatching
          (\case InvestigatorWhenDefeated _ iid' -> iid == iid'; _ -> False)
          \case
            InvestigatorWhenDefeated source' _ -> [Msg.checkDefeated source' iid]
            _ -> error "invalid match"
        Msg.removeAllMessagesMatching \case
          AssignDamage (InvestigatorTarget iid') -> iid == iid'
          AssignedDamage (InvestigatorTarget iid') _ _ -> iid == iid'
          _ -> False
      pushWhenM (canHaveHorrorHealed attrs iid) $ HealHorror (toTarget iid) (toSource attrs) 5
      pushWhenM (canHaveDamageHealed attrs iid) $ HealDamage (toTarget iid) (toSource attrs) 5
      chooseOne iid [CardLabel "10016a" False [DoStep 1 msg], CardLabel "10016b" False [DoStep 2 msg]]
      pure
        . HankSamson
        $ attrs {investigatorAssignedHealthDamage = 0, investigatorAssignedSanityDamage = 0}
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      case investigatorForm attrs of
        TransfiguredForm _ ->
          pure . HankSamson $ attrs {investigatorForm = TransfiguredForm "10016a"}
        _ ->
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
      case investigatorForm attrs of
        TransfiguredForm _ ->
          pure . HankSamson $ attrs {investigatorForm = TransfiguredForm "10016b"}
        _ ->
          pure
            . HankSamson
            $ attrs
              { investigatorArt = "10016b"
              , investigatorHealth = 6
              , investigatorSanity = 4
              , investigatorWillpower = 4
              , investigatorCombat = 6
              }
    UseThisAbility iid (isSource attrs -> True) 1 | hankSide attrs == "10016a" -> do
      drawCards iid attrs 1
      pure i
    UseThisAbility iid (isSource attrs -> True) 1 | hankSide attrs == "10016b" -> do
      gainResources iid attrs 2
      pure i
    ElderSignEffect iid | iid == attrs.id -> do
      case hankSide attrs of
        "10016a" | attrs.sanityDamage > 0 -> do
          canHorrorAssets <- select $ assetControlledBy iid <> AssetWithAnyRemainingSanity
          unless (null canHorrorAssets) do
            chooseOne
              iid
              [ targetLabel asset [MoveTokens (toSource attrs) (toSource iid) (toTarget asset) #horror 1]
              | asset <- canHorrorAssets
              ]
        "10016b" | attrs.healthDamage > 0 -> do
          canDamageAssets <- select $ assetControlledBy iid <> AssetWithAnyRemainingHealth
          unless (null canDamageAssets) do
            chooseOne
              iid
              [ targetLabel asset [MoveTokens (toSource attrs) (toSource iid) (toTarget asset) #damage 1]
              | asset <- canDamageAssets
              ]
        _ -> pure ()
      pure i
    ForInvestigators _ ResetGame | investigatorCardCode attrs == "10015" -> do
      HankSamson
        <$> liftRunMessage
          msg
          ( attrs
              { investigatorArt = "10015"
              , investigatorHealth = 5
              , investigatorSanity = 5
              , investigatorWillpower = 3
              , investigatorIntellect = 1
              , investigatorCombat = 5
              , investigatorAgility = 3
              }
          )
    _ -> HankSamson <$> liftRunMessage msg attrs

module Arkham.Investigator.Cards.DianaStanley (
  dianaStanley,
  DianaStanley (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Event.Cards qualified as Events
import Arkham.Event.Types (Field (..))
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (AssetCard, EventCard, PlaceUnderneath, SkillCard)
import Arkham.Message
import Arkham.Projection
import Arkham.Skill.Types (Field (..))
import Arkham.SkillType
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DianaStanley = DianaStanley InvestigatorAttrs
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dianaStanley :: InvestigatorCard DianaStanley
dianaStanley =
  investigatorWith
    DianaStanley
    Cards.dianaStanley
    Stats
      { health = 7
      , sanity = 7
      , willpower = 1
      , intellect = 3
      , combat = 3
      , agility = 3
      }
    (startsWithInHandL .~ [Events.darkInsight])

instance HasModifiersFor DianaStanley where
  getModifiersFor target (DianaStanley a) | isTarget a target = do
    n <- fieldMap InvestigatorCardsUnderneath length (toId a)
    pure $ toModifiers a [SkillModifier SkillWillpower n | n > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities DianaStanley where
  getAbilities (DianaStanley a) =
    [ limitedAbility (PlayerLimit PerPhase 1) $
        restrictedAbility a 1 (Self <> fewerThan5CardBeneath) $
          ReactionAbility
            ( CancelledOrIgnoredCardOrGameEffect $
                SourceOwnedBy You
                  <> NotSource
                    (SourceIsType InvestigatorType)
            )
            Free
    ]
   where
    fewerThan5CardBeneath =
      if length (investigatorCardsUnderneath a) < 5
        then NoRestriction
        else Never

instance HasChaosTokenValue DianaStanley where
  getChaosTokenValue iid ElderSign (DianaStanley attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage DianaStanley where
  runMessage msg i@(DianaStanley attrs) = case msg of
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      cardsUnderneath <- field InvestigatorCardsUnderneath iid
      unless (null cardsUnderneath) $ do
        pushAll $
          [ FocusCards cardsUnderneath
          , chooseOrRunOne iid $
              Label "Do not add any cards to your Hand" []
                : [ TargetLabel (CardIdTarget $ toCardId c) [addToHand iid c]
                  | c <- cardsUnderneath
                  ]
          , UnfocusCards
          ]
      pure i
    UseCardAbility
      iid
      (isSource attrs -> True)
      1
      [(windowType -> Window.CancelledOrIgnoredCardOrGameEffect source)]
      _ ->
        do
          let
            getSource = \case
              AssetSource aid -> (RemoveAsset aid,) <$> field AssetCard aid
              SkillSource sid -> (RemoveSkill sid,) <$> field SkillCard sid
              EventSource eid -> (RemoveEvent eid,) <$> field EventCard eid
              AbilitySource abilitySource _ -> getSource abilitySource
              CardSource card -> pure (RemovePlayerCardFromGame False card, card)
              _ -> error $ "Unhandled source for Diana Stanley: " <> show source
          (removeMsg, card) <- getSource source
          canLeavePlay <- case source of
            AssetSource aid ->
              member aid <$> select AssetCanLeavePlayByNormalMeans
            _ -> pure $ not (cdPermanent $ toCardDef card)
          drawing <- drawCards iid (toAbilitySource attrs 1) 1
          pushAll $
            [removeMsg | canLeavePlay]
              <> [PlaceUnderneath (toTarget attrs) [card] | canLeavePlay]
              <> [drawing, TakeResources iid 1 (toAbilitySource attrs 1) False]
          pure i
    _ -> DianaStanley <$> runMessage msg attrs

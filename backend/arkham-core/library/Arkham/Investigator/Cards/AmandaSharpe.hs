module Arkham.Investigator.Cards.AmandaSharpe (
  amandaSharpe,
  AmandaSharpe (..),
)
where

import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Prelude
import Arkham.Projection

newtype AmandaSharpe = AmandaSharpe InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amandaSharpe :: InvestigatorCard AmandaSharpe
amandaSharpe =
  investigatorWith
    AmandaSharpe
    Cards.amandaSharpe
    Stats
      { health = 7
      , sanity = 7
      , willpower = 2
      , intellect = 2
      , combat = 2
      , agility = 2
      }
    $ setMeta @(Maybe CardId) Nothing

instance HasAbilities AmandaSharpe where
  getAbilities (AmandaSharpe attrs) =
    [ restrictedAbility attrs 1 Self $ forced $ PhaseBegins #when #investigation
    , restrictedAbility attrs 2 Self $ forced $ InitiatedSkillTest #when You #any #any #any
    ]

instance HasChaosTokenValue AmandaSharpe where
  getChaosTokenValue iid ElderSign (AmandaSharpe attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AmandaSharpe where
  runMessage msg i@(AmandaSharpe attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mdrawing <- drawCardsIfCan attrs.id attrs 1
      for_ mdrawing push
      let meta = toResult @(Maybe CardId) attrs.meta
      for_ meta $ push . DiscardCard iid (toSource attrs)
      push $ DoStep 1 msg
      pure i
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      hand <- field InvestigatorHand iid
      player <- getPlayer iid
      pushWhen (notNull hand) $ do
        chooseOrRunOne
          player
          [ targetLabel
            (CardIdTarget $ toCardId card)
            [HandleTargetChoice iid (attrs.ability 1) (CardIdTarget $ toCardId card)]
          | card <- hand
          ]
      pure i
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      push $ PlaceUnderneath (toTarget iid) [card]
      pure $ AmandaSharpe $ attrs & setMeta @(Maybe CardId) (Just cid)
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let meta = toResult @(Maybe CardId) attrs.meta
      for_ meta $ \cardId -> do
        card <- getCard cardId
        committable <- getIsCommittable iid card
        when committable do
          pushAll
            [ skillTestModifiers (toSource attrs) cardId [MustBeCommitted, LeaveCardWhereItIs]
            , SkillTestCommitCard iid card
            ]
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      let meta = toResult @(Maybe CardId) attrs.meta
      player <- getPlayer iid
      for_ meta $ \cardId -> do
        push
          $ chooseOne
            player
            [ Label "Double skill icons" [skillTestModifier (toSource attrs) cardId DoubleSkillIcons]
            , Label "Do not double skill icons" []
            ]
      pure i
    _ -> AmandaSharpe <$> lift (runMessage msg attrs)

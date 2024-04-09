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
import Arkham.Skill.Cards qualified as Skills

newtype AmandaSharpe = AmandaSharpe InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amandaSharpe :: InvestigatorCard AmandaSharpe
amandaSharpe =
  investigator
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

instance HasAbilities AmandaSharpe where
  getAbilities (AmandaSharpe attrs) =
    [ restrictedAbility attrs 1 Self $ forced $ PhaseBegins #when #investigation
    , restrictedAbility attrs 2 Self $ forced $ InitiatedSkillTest #at You #any #any #any
    ]

instance HasChaosTokenValue AmandaSharpe where
  getChaosTokenValue iid ElderSign (AmandaSharpe attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AmandaSharpe where
  runMessage msg i@(AmandaSharpe attrs) = runQueueT $ case msg of
    SetupInvestigator iid | iid == attrs.id -> do
      attrs' <- lift (runMessage msg attrs)
      pure . AmandaSharpe $ attrs' & setMeta @(Maybe CardId) Nothing
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mdrawing <- drawCardsIfCan attrs.id attrs 1
      for_ mdrawing push
      let mCard =
            preview _PlayerCard
              =<< (\cardId -> find ((== cardId) . toCardId) (investigatorCardsUnderneath attrs))
              =<< toResult attrs.meta
      for_ mCard \card -> do
        push $ ObtainCard (toCard card)
        push $ AddToDiscard iid card
      push $ DoStep 1 msg
      pure $ AmandaSharpe $ attrs & setMeta @(Maybe CardId) Nothing
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      hand <- field InvestigatorHand iid
      let mWhispersFromTheDeep = find (`cardMatch` cardIs Skills.whispersFromTheDeep) hand
      player <- getPlayer iid
      case mWhispersFromTheDeep of
        Nothing -> do
          pushWhen (notNull hand) $ do
            chooseOrRunOne
              player
              [ targetLabel
                (CardIdTarget $ toCardId card)
                [HandleTargetChoice iid (attrs.ability 1) (CardIdTarget $ toCardId card)]
              | card <- hand
              ]
        Just whispersFromTheDeep -> do
          push
            $ chooseOne
              player
              [ AbilityLabel
                  iid
                  ( mkAbility
                      (proxy (CardIdSource $ toCardId whispersFromTheDeep) attrs)
                      1
                      (ForcedAbility AnyWindow)
                  )
                  []
                  [HandleTargetChoice iid (attrs.ability 1) (CardIdTarget $ toCardId whispersFromTheDeep)]
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

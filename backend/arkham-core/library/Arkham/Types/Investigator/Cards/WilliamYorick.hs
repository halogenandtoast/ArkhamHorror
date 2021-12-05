module Arkham.Types.Investigator.Cards.WilliamYorick where

import Arkham.Prelude

import Arkham.Investigator.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype WilliamYorick = WilliamYorick InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamYorick :: InvestigatorCard WilliamYorick
williamYorick = investigator
  WilliamYorick
  Cards.williamYorick
  Stats
    { health = 8
    , sanity = 6
    , willpower = 3
    , intellect = 2
    , combat = 4
    , agility = 3
    }

instance HasTokenValue env WilliamYorick where
  getTokenValue (WilliamYorick attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance HasAbilities WilliamYorick where
  getAbilities (WilliamYorick attrs) =
    [ restrictedAbility
          attrs
          1
          (Self
          <> PlayableCardInDiscard (DiscardOf You) (CardWithType AssetType)
          )
          (ReactionAbility (EnemyDefeated Timing.After You AnyEnemy) Free)
        & (abilityLimitL .~ PlayerLimit PerRound 1)
    ]

instance (InvestigatorRunner env) => RunMessage env WilliamYorick where
  runMessage msg i@(WilliamYorick attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      let
        targets =
          filter ((== AssetType) . toCardType) (investigatorDiscard attrs)
        playCardMsgs c = [AddToHand iid c] <> if isDynamic c
          then [InitiatePlayDynamicCard iid (toCardId c) 0 Nothing False]
          else if isFastCard c
            then [InitiatePlayCard iid (toCardId c) Nothing False]
            else
              [ PayCardCost iid (toCardId c)
              , InitiatePlayCard iid (toCardId c) Nothing False
              ]
      playableTargets <- filterM
        (getIsPlayable
            iid
            source
            UnpaidCost
            [ Window Timing.When Window.NonFast
            , Window Timing.When (Window.DuringTurn iid)
            ]
        . PlayerCard
        )
        targets
      i <$ push
        (chooseOne iid
        $ [ TargetLabel
              (CardIdTarget $ toCardId card)
              (playCardMsgs $ PlayerCard card)
          | card <- playableTargets
          ]
        )
    ResolveToken _ ElderSign iid | iid == toId attrs -> do
      i <$ push
        (CreateEffect
          (unInvestigatorId $ toId attrs)
          Nothing
          (TokenEffectSource ElderSign)
          (InvestigatorTarget iid)
        )
    _ -> WilliamYorick <$> runMessage msg attrs

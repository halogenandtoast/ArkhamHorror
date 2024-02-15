module Arkham.Treachery.Cards.ChillingPresence (
  chillingPresence,
  chillingPresenceEffect,
  ChillingPresence (..),
)
where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Effect.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Trait (Trait (Geist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ChillingPresence = ChillingPresence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillingPresence :: TreacheryCard ChillingPresence
chillingPresence = treachery ChillingPresence Cards.chillingPresence

instance RunMessage ChillingPresence where
  runMessage msg t@(ChillingPresence attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      pushAll
        [ createCardEffect Cards.chillingPresence Nothing (toSource attrs) (toTarget iid)
        , beginSkillTest iid (toSource attrs) iid #willpower 3
        ]

      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ assignHorror iid (toSource attrs) n
      pure t
    PassedThisSkillTestBy _ (isSource attrs -> True) n -> do
      push $ DoStep n msg
      pure t
    DoStep n msg'@(PassedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      geists <- select $ enemyAtLocationWith iid <> EnemyWithTrait Geist
      when (notNull geists) $ do
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [ Label
                "Deal 1 damage to a Geist enemy at your location"
                [ chooseOrRunOne
                    player
                    [ targetLabel geist [EnemyDamage geist $ nonAttack (toSource attrs) 1, DoStep (n - 1) msg']
                    | geist <- geists
                    ]
                ]
            , Label "Skip" []
            ]
      pure t
    _ -> ChillingPresence <$> runMessage msg attrs

newtype ChillingPresenceEffect = ChillingPresenceEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillingPresenceEffect :: EffectArgs -> ChillingPresenceEffect
chillingPresenceEffect = cardEffect ChillingPresenceEffect Cards.chillingPresence

instance RunMessage ChillingPresenceEffect where
  runMessage msg e@(ChillingPresenceEffect attrs) = case msg of
    Msg.RevealChaosToken _ iid token -> do
      when (chaosTokenFace token == #eldersign) $ do
        geists <- select $ EnemyWithTrait Geist
        when (notNull geists) $ do
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [ Label
                  "Deal 2 damage to a Geist enemy at any location"
                  [ chooseOrRunOne
                      player
                      [ targetLabel geist [EnemyDamage geist $ nonAttack (toSource attrs) 2]
                      | geist <- geists
                      ]
                  ]
              , Label "Skip" []
              ]
      pure e
    SkillTestEnds _ _ -> do
      push $ disable attrs
      pure e
    _ -> ChillingPresenceEffect <$> runMessage msg attrs

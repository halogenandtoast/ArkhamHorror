module Arkham.Agenda.Cards.TheTrueCulpritV10 (
  TheTrueCulpritV10 (..),
  theTrueCulpritV10,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Cultist, Guest, Innocent, Lead))

newtype TheTrueCulpritV10 = TheTrueCulpritV10 AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV10 :: AgendaCard TheTrueCulpritV10
theTrueCulpritV10 = agenda (3, A) TheTrueCulpritV10 Cards.theTrueCulpritV10 (Static 12)

instance HasModifiersFor TheTrueCulpritV10 where
  getModifiersFor (EnemyTarget eid) (TheTrueCulpritV10 attrs) = do
    isGuest <- eid <=~> EnemyWithTrait Guest
    pure $ toModifiers attrs $ guard isGuest *> [LoseVictory, RemoveTrait Innocent, AddTrait Cultist]
  getModifiersFor _ _ = pure []

instance HasAbilities TheTrueCulpritV10 where
  getAbilities (TheTrueCulpritV10 attrs) =
    guard (onSide A attrs)
      *> [ restrictedAbility (proxy (locationIs Cards.basement) attrs) 1 Here actionAbility
         , restrictedAbility
            attrs
            2
            (exists $ AgendaWithDoom (EqualTo $ Static 0) <> AgendaWithId (toId attrs))
            $ Objective
            $ ForcedAbility AnyWindow
         ]

instance RunMessage TheTrueCulpritV10 where
  runMessage msg a@(TheTrueCulpritV10 attrs) =
    case msg of
      UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
        player <- getPlayer iid
        leadAssets <- select $ AssetWithTrait Lead <> assetControlledBy iid
        pushAll
          $ [ chooseOne
                player
                [ Label
                    "remove 1 clue from a Lead asset in the Basement to reduce the dicculty of this test by 2"
                    [ chooseOrRunOne
                        player
                        [ targetLabel
                          asset
                          [ RemoveClues (AbilitySource p 1) (toTarget asset) 1
                          , skillTestModifier (AbilitySource p 1) SkillTestTarget (Difficulty (-2))
                          ]
                        | asset <- leadAssets
                        ]
                    ]
                , Label "Skip" []
                ]
            ]
          <> [ chooseOne
                player
                [ SkillLabel skill [beginSkillTest iid (AbilitySource p 1) iid skill (Fixed 4)]
                | skill <- [#willpower, #agility]
                ]
             ]
        pure a
      PassedThisSkillTest _ p@(isProxyAbilitySource attrs 1 -> True) -> do
        push $ RemoveDoom p (toTarget attrs) 1
        pure a
      UseThisAbility _ (isSource attrs -> True) 2 -> do
        push $ AdvanceAgendaBy (toId attrs) AgendaAdvancedWithOther
        pure a
      AdvanceAgendaBy aid AgendaAdvancedWithDoom | aid == toId attrs && onSide B attrs -> do
        push R2
        pure a
      AdvanceAgendaBy aid AgendaAdvancedWithOther | aid == toId attrs && onSide B attrs -> do
        push R1
        pure a
      _ -> TheTrueCulpritV10 <$> runMessage msg attrs

module Arkham.Agenda.Cards.SpecialInvestigation (
  SpecialInvestigation (..),
  specialInvestigation,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner hiding (DiscoverClues)
import Arkham.Attack
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Trait (Trait (Humanoid, Police))

newtype SpecialInvestigation = SpecialInvestigation AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

specialInvestigation :: AgendaCard SpecialInvestigation
specialInvestigation = agenda (2, A) SpecialInvestigation Cards.specialInvestigation (Static 12)

instance HasAbilities SpecialInvestigation where
  getAbilities (SpecialInvestigation a) =
    [ mkAbility a 1
        $ ForcedAbility
        $ oneOf
          [ DiscoverClues
              #after
              You
              ( LocationWithEnemy
                  ( EnemyWithTrait Police
                      <> ReadyEnemy
                  )
                  <> LocationWithInvestigator You
              )
              (atLeast 1)
          , EnemyDealtDamage
              #after
              AnyDamageEffect
              ( EnemyWithTrait Humanoid
                  <> EnemyAt
                    ( LocationWithEnemy
                        ( EnemyWithTrait Police
                            <> ReadyEnemy
                        )
                        <> LocationWithInvestigator You
                    )
              )
              (SourceOwnedBy You)
          ]
    ]

instance RunMessage SpecialInvestigation where
  runMessage msg a@(SpecialInvestigation attrs) =
    case msg of
      UseThisAbility iid (isSource attrs -> True) 1 -> do
        police <- selectList $ enemyAtLocationWith iid <> ReadyEnemy <> EnemyWithTrait Police
        player <- getPlayer iid
        push
          $ chooseOneAtATime
            player
            [ targetLabel
              officer
              [EnemyEngageInvestigator officer iid, EnemyAttack $ enemyAttack officer attrs iid]
            | officer <- police
            ]

        pure a
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        push R3
        pure a
      _ -> SpecialInvestigation <$> runMessage msg attrs

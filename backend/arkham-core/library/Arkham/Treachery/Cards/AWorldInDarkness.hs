module Arkham.Treachery.Cards.AWorldInDarkness (
  aWorldInDarkness,
  AWorldInDarkness (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype AWorldInDarkness = AWorldInDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aWorldInDarkness :: TreacheryCard AWorldInDarkness
aWorldInDarkness = treachery AWorldInDarkness Cards.aWorldInDarkness

instance RunMessage AWorldInDarkness where
  runMessage msg t@(AWorldInDarkness attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      doom <- field EnemyDoom azathoth
      if doom == 0
        then push $ GainSurge (toSource attrs) (toTarget attrs)
        else do
          hasResources <- fieldMap InvestigatorResources (>= 1) iid
          canDiscard <- iid <=~> InvestigatorWithDiscardableCard
          pushAll
            $ replicate doom
            $ chooseOne iid
            $ [Label "Lose 1 resource" [LoseResources iid (toSource attrs) 1] | hasResources]
            <> [ Label "Choose and discard 1 card from your hand" [toMessage $ chooseAndDiscardCard iid attrs]
               | canDiscard
               ]
            <> [ Label "Take 1 horror" [assignHorror iid attrs 1]
               , Label "Take 1 damage" [assignDamage iid attrs 1]
               ]
      pure t
    _ -> AWorldInDarkness <$> runMessage msg attrs

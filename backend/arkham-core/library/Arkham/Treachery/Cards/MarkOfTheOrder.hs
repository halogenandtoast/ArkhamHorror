module Arkham.Treachery.Cards.MarkOfTheOrder (
  markOfTheOrder,
  MarkOfTheOrder (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Key
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MarkOfTheOrder = MarkOfTheOrder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

markOfTheOrder :: TreacheryCard MarkOfTheOrder
markOfTheOrder = treachery MarkOfTheOrder Cards.markOfTheOrder

instance RunMessage MarkOfTheOrder where
  runMessage msg t@(MarkOfTheOrder attrs) = case msg of
    Revelation _ (isSource attrs -> True) -> do
      skullInvestigator <- selectOne $ InvestigatorWithKey SkullKey
      cultistInvestigator <- selectOne $ InvestigatorWithKey CultistKey
      tabletInvestigator <- selectOne $ InvestigatorWithKey TabletKey
      elderThingInvestigator <- selectOne $ InvestigatorWithKey ElderThingKey

      for_ elderThingInvestigator $ \i ->
        push $ LoseResources i (toSource attrs) 3

      for_ tabletInvestigator $ \i ->
        push $ toMessage $ randomDiscard i attrs

      for_ cultistInvestigator $ \i ->
        push $ InvestigatorAssignDamage i (toSource attrs) DamageAny 0 1

      for_ skullInvestigator $ \i ->
        push $ InvestigatorAssignDamage i (toSource attrs) DamageAny 1 0

      pure t
    _ -> MarkOfTheOrder <$> runMessage msg attrs

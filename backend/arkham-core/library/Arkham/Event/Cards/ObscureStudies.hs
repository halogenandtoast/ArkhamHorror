module Arkham.Event.Cards.ObscureStudies (
  obscureStudies,
  ObscureStudies (..),
)
where

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype ObscureStudies = ObscureStudies EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obscureStudies :: EventCard ObscureStudies
obscureStudies = eventWith ObscureStudies Cards.obscureStudies (afterPlayL .~ RemoveThisFromGame)

instance RunMessage ObscureStudies where
  runMessage msg e@(ObscureStudies attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      meta <- toResult @(Maybe CardId) <$> field InvestigatorMeta iid
      for_ meta \cardId -> do
        pushAll
          [ ReturnToHand iid (CardIdTarget cardId)
          , RemoveEvent attrs.id
          , HandleTargetChoice iid (toAbilitySource iid 1) (CardIdTarget $ toCardId attrs)
          ]

      pure e
    _ -> ObscureStudies <$> runMessage msg attrs

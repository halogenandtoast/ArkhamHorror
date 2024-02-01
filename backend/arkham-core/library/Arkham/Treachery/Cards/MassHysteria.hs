module Arkham.Treachery.Cards.MassHysteria (
  massHysteria,
  MassHysteria (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MassHysteria = MassHysteria TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

massHysteria :: TreacheryCard MassHysteria
massHysteria = treachery MassHysteria Cards.massHysteria

instance RunMessage MassHysteria where
  runMessage msg t@(MassHysteria attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      hasLocation <- isJust <$> field InvestigatorLocation iid
      anyMaskedCarnevaleGoers <-
        selectAny
          (AssetWithTitle "Masked Carnevale-Goer")
      player <- getPlayer iid
      let take2damage = InvestigatorAssignDamage iid source DamageAny 2 0
      if hasLocation && anyMaskedCarnevaleGoers
        then
          push
            $ chooseOne
              player
              [ Label "Take 2 damage" [take2damage]
              , Label "Shuffle Masked Carnevale-Goers" [RevelationChoice iid source 2]
              ]
        else push take2damage
      pure t
    RevelationChoice iid source 2 | isSource attrs source -> do
      locationId <- fromJustNote "impossible" <$> field InvestigatorLocation iid
      maskedCarnevaleGoers <- selectList (AssetWithTitle "Masked Carnevale-Goer")
      clockwiseLocations <- getClockwiseLocations locationId
      shuffled <- shuffleM maskedCarnevaleGoers
      pushAll
        [ AttachAsset aid (LocationTarget lid)
        | (aid, lid) <- zip shuffled clockwiseLocations
        ]
      pure t
    _ -> MassHysteria <$> runMessage msg attrs

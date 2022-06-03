module Arkham.Treachery.Cards.MassHysteria
  ( massHysteria
  , MassHysteria(..)
  ) where

import Arkham.Prelude

import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype MassHysteria = MassHysteria TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

massHysteria :: TreacheryCard MassHysteria
massHysteria = treachery MassHysteria Cards.massHysteria

instance TreacheryRunner env => RunMessage MassHysteria where
  runMessage msg t@(MassHysteria attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ push
      (chooseOne
        iid
        [ Label
          "Take 2 damage"
          [InvestigatorAssignDamage iid source DamageAny 2 0]
        , Label "Shuffle Masked Carnevale-Goers" [RevelationChoice iid source 2]
        ]
      )
    RevelationChoice iid source 2 | isSource attrs source -> do
      locationId <- getId @LocationId iid
      maskedCarnevaleGoers <- selectList
        (AssetWithTitle "Masked Carnevale-Goer")
      clockwiseLocations <- getClockwiseLocations locationId
      case maskedCarnevaleGoers of
        [] -> pure t
        xs -> do
          shuffled <- shuffleM xs
          t <$ pushAll
            [ AttachAsset aid (LocationTarget lid)
            | (aid, lid) <- zip shuffled clockwiseLocations
            ]
    _ -> MassHysteria <$> runMessage msg attrs

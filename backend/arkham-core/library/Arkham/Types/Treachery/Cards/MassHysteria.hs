module Arkham.Types.Treachery.Cards.MassHysteria
  ( massHysteria
  , MassHysteria(..)
  ) where

import Arkham.Prelude

import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype MassHysteria = MassHysteria TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

massHysteria :: TreacheryCard MassHysteria
massHysteria = treachery MassHysteria Cards.massHysteria

instance TreacheryRunner env => RunMessage env MassHysteria where
  runMessage msg t@(MassHysteria attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [ chooseOne
        iid
        [ Label
          "Take 2 damage"
          [InvestigatorAssignDamage iid source DamageAny 2 0]
        , Label "Shuffle Masked Carnevale-Goers" [RevelationChoice iid source 2]
        ]
      , Discard (toTarget attrs)
      ]
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

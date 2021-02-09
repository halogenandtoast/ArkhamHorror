module Arkham.Types.Location.Cards.ArtGallery
  ( artGallery
  , ArtGallery(..)
  ) where


import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait hiding (Cultist)

newtype ArtGallery = ArtGallery LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artGallery :: ArtGallery
artGallery = ArtGallery
  $ base { locationVictory = Just 1, locationRevealedSymbol = Hourglass }
 where
  base = baseAttrs
    "02075"
    (Name "Art Gallery" Nothing)
    EncounterSet.TheHouseAlwaysWins
    2
    (PerPlayer 1)
    T
    [Diamond]
    [CloverClub]

instance HasModifiersFor env ArtGallery where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArtGallery where
  getActions iid window (ArtGallery attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env ArtGallery where
  runMessage msg l@(ArtGallery attrs@LocationAttrs {..}) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ (SkillTestInitiatorTarget _) _ _)
      -> l <$ unshiftMessage (SpendResources iid 2)
    _ -> ArtGallery <$> runMessage msg attrs

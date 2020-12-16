{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArtGallery
  ( artGallery
  , ArtGallery(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait hiding (Cultist)

newtype ArtGallery = ArtGallery Attrs
  deriving newtype (Show, ToJSON, FromJSON)

artGallery :: ArtGallery
artGallery = ArtGallery $ (baseAttrs
                            "02075"
                            "Art Gallery"
                            EncounterSet.TheHouseAlwaysWins
                            2
                            (PerPlayer 1)
                            T
                            [Diamond]
                            [CloverClub]
                          )
  { locationVictory = Just 1
  , locationRevealedSymbol = Hourglass
  }

instance HasModifiersFor env ArtGallery where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArtGallery where
  getActions iid window (ArtGallery attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env ArtGallery where
  runMessage msg l@(ArtGallery attrs@Attrs {..}) = case msg of
    FailedSkillTest iid (Just Action.Investigate) _ (SkillTestInitiatorTarget _) _
      -> l <$ unshiftMessage (SpendResources iid 2)
    _ -> ArtGallery <$> runMessage msg attrs

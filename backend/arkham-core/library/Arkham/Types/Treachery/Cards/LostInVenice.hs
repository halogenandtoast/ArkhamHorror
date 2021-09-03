module Arkham.Types.Treachery.Cards.LostInVenice
  ( lostInVenice
  , LostInVenice(..)
  ) where

import Arkham.Prelude

import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype LostInVenice = LostInVenice TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInVenice :: TreacheryCard LostInVenice
lostInVenice = treachery LostInVenice Cards.lostInVenice

instance TreacheryRunner env => RunMessage env LostInVenice where
  runMessage msg t@(LostInVenice attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      acrossLocationId <- getAcrossLocation lid
      t <$ push
        (chooseOne
          iid
          [ Label
            "Take 2 damage"
            [InvestigatorAssignDamage iid source DamageAny 2 0]
          , Label
            "Move to the location across from you"
            [Move source iid lid acrossLocationId]
          ]
        )
    _ -> LostInVenice <$> runMessage msg attrs

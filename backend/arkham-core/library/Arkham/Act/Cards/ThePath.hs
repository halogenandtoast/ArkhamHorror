module Arkham.Act.Cards.ThePath (
  ThePath (..),
  thePath,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Matcher

newtype ThePath = ThePath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

thePath :: ActCard ThePath
thePath = act (4, A) ThePath Cards.thePath (Just $ GroupClueCost (PerPlayer 5) "The Enchanted Path")

instance HasAbilities ThePath where
  getAbilities (ThePath x) =
    withBaseAbilities
      x
      [ restrictedAbility x 1 (NotYetRecorded TheDreamersStrayedFromThePath)
          $ ForcedAbility
          $ Enters #after Anyone
          $ LocationWithUnrevealedTitle "Enchanted Woods"
      ]

instance RunMessage ThePath where
  runMessage msg a@(ThePath attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ Record TheDreamersStrayedFromThePath
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      push
        $ chooseOne
          lead
          [ Label "Step back and watch this surreal scene play out." [R1]
          , Label "Interrupt the scarred cat and handle this yourself." [R2]
          ]
      pure a
    _ -> ThePath <$> runMessage msg attrs

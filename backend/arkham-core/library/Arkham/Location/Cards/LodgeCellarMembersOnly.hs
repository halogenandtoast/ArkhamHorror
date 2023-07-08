module Arkham.Location.Cards.LodgeCellarMembersOnly (
  lodgeCellarMembersOnly,
  LodgeCellarMembersOnly (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message

newtype LodgeCellarMembersOnly = LodgeCellarMembersOnly LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeCellarMembersOnly :: LocationCard LodgeCellarMembersOnly
lodgeCellarMembersOnly = location LodgeCellarMembersOnly Cards.lodgeCellarMembersOnly 3 (Static 0)

instance HasModifiersFor LodgeCellarMembersOnly where
  getModifiersFor target (LodgeCellarMembersOnly attrs)
    | isTarget attrs target =
        pure $ toModifiers attrs [Blocked | not (locationRevealed attrs)]
  getModifiersFor _ _ = pure []

instance HasAbilities LodgeCellarMembersOnly where
  getAbilities (LodgeCellarMembersOnly attrs) =
    withBaseAbilities
      attrs
      [ withTooltip
        "{action}: Investigators at the Lodge Gates spend 1 {perPlayer} clues, as a group: Reveal the Lodge Cellar."
        $ restrictedAbility
          ( ProxySource
              (LocationMatcherSource $ LocationWithTitle "Lodge Gates")
              (toSource attrs)
          )
          1
          (OnLocation $ LocationWithTitle "Lodge Gates")
          ( ActionAbility Nothing $
              ActionCost 1 <> GroupClueCost (PerPlayer 1) (LocationWithTitle "Lodge Gates")
          )
      | unrevealed attrs
      ]

instance RunMessage LodgeCellarMembersOnly where
  runMessage msg l@(LodgeCellarMembersOnly attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) 1 _ _
      | isSource attrs source && unrevealed attrs -> do
          push $ RevealLocation (Just iid) (toId attrs)
          pure l
    _ -> LodgeCellarMembersOnly <$> runMessage msg attrs

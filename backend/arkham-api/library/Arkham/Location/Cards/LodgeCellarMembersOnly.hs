module Arkham.Location.Cards.LodgeCellarMembersOnly (
  lodgeCellarMembersOnly,
  LodgeCellarMembersOnly (..),
)
where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message

newtype LodgeCellarMembersOnly = LodgeCellarMembersOnly LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeCellarMembersOnly :: LocationCard LodgeCellarMembersOnly
lodgeCellarMembersOnly = location LodgeCellarMembersOnly Cards.lodgeCellarMembersOnly 3 (Static 0)

instance HasModifiersFor LodgeCellarMembersOnly where
  getModifiersFor (LodgeCellarMembersOnly a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities LodgeCellarMembersOnly where
  getAbilities (LodgeCellarMembersOnly attrs) =
    extend
      attrs
      [ withTooltip
        "{action}: Investigators at the Lodge Gates spend 1 {perPlayer} clues, as a group: Reveal the Lodge Cellar."
        $ restricted (proxied (LocationMatcherSource "Lodge Gates") attrs) 1 Here
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (LocationWithTitle "Lodge Gates"))
      | attrs.unrevealed
      ]

instance RunMessage LodgeCellarMembersOnly where
  runMessage msg l@(LodgeCellarMembersOnly attrs) = runQueueT $ case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      push $ RevealLocation (Just iid) attrs.id
      pure l
    _ -> LodgeCellarMembersOnly <$> liftRunMessage msg attrs

module Arkham.Homebrew.AgesUnwound.Acts.SlippingTheNet (slippingTheNet) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Matcher

newtype SlippingTheNet = SlippingTheNet ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slippingTheNet :: ActCard SlippingTheNet
slippingTheNet = act (3, A) SlippingTheNet Cards.slippingTheNet Nothing

-- | "Each location gets +1 shroud."
instance HasModifiersFor SlippingTheNet where
  getModifiersFor (SlippingTheNet a) = modifySelect a Anywhere [ShroudModifier 1]

{- | "Objective - If there are no clues on A Place to Hide, Winchmore House and
Cramped Passage, advance." Act 2b put 1[per_investigator] clues on each of the
three, so this cannot be vacuously true the moment the act enters play.
-}
instance HasAbilities SlippingTheNet where
  getAbilities (SlippingTheNet a) =
    [ restricted
        a
        1
        ( notExists
            $ LocationWithAnyClues
            <> mapOneOf
              locationIs
              [Locations.aPlaceToHide, Locations.winchmoreHouse, Locations.crampedPassage]
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SlippingTheNet where
  runMessage msg a@(SlippingTheNet attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    -- "Sweet Freedom. -> R4"
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R4
      pure a
    _ -> SlippingTheNet <$> liftRunMessage msg attrs

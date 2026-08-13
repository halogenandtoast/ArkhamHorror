module Arkham.Homebrew.DarkMatter.Acts.IsAnyoneHome (isAnyoneHome) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (scanEventForCardType, scenarioI18n)
import Arkham.Matcher

newtype IsAnyoneHome = IsAnyoneHome ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isAnyoneHome :: ActCard IsAnyoneHome
isAnyoneHome = act (1, A) IsAnyoneHome Cards.isAnyoneHome Nothing

-- "Objective - When you draw a story asset from the scanning deck, advance."
instance HasAbilities IsAnyoneHome where
  getAbilities (IsAnyoneHome a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ CampaignEvent #after Nothing (scanEventForCardType AssetType)
    ]

instance RunMessage IsAnyoneHome where
  runMessage msg a@(IsAnyoneHome attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    -- Act 1b, "In Hiding": "Reveal the Ship Mainframe location. Advance to act
    -- 2a." Setup already put the Ship Mainframe into play unrevealed.
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      scenarioI18n "inTheShadowOfEarth" $ scope "act1b" do
        flavor $ setTitle "title" >> p "body"
      selectOne (locationIs Locations.shipMainframe) >>= traverse_ reveal
      advanceActDeck attrs
      pure a
    _ -> IsAnyoneHome <$> liftRunMessage msg attrs

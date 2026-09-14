module Arkham.Homebrew.DarkMatter.Locations.Yuggoth (yuggoth) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Pluto, pattern Starship)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Yuggoth = Yuggoth LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yuggoth :: LocationCard Yuggoth
yuggoth = symbolLabel $ location Yuggoth Cards.yuggoth 2 (Static 1)

{- | "During your turn, if you are at a [[Pluto]] location or a [[Starship]]
location attached to a [[Pluto]] location, reveal and resolve an additional chaos
token during skill tests you perform."
-}
instance HasModifiersFor Yuggoth where
  getModifiersFor (Yuggoth a) =
    modifySelect
      a
      ( TurnInvestigator
          <> InvestigatorAt
            ( oneOf
                [ LocationWithTrait Pluto
                , LocationWithTrait Starship <> connectedTo (LocationWithTrait Pluto)
                ]
            )
      )
      [RevealAnotherChaosToken]

-- | "Forced - After you reveal Yuggoth: Spawn the set aside Mi-Go Sentinel at this location."
instance HasAbilities Yuggoth where
  getAbilities (Yuggoth a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage Yuggoth where
  runMessage msg l@(Yuggoth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      createEnemyAt_ Enemies.miGoSentinel attrs.id
      pure l
    _ -> Yuggoth <$> liftRunMessage msg attrs

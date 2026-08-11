module Arkham.Homebrew.DarkMatter.Locations.MoonbaseLaboratory (moonbaseLaboratory) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Starship)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token qualified as Token

newtype MoonbaseLaboratory = MoonbaseLaboratory LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonbaseLaboratory :: LocationCard MoonbaseLaboratory
moonbaseLaboratory = location MoonbaseLaboratory Cards.moonbaseLaboratory 3 (PerPlayer 2)

-- | "You cannot scan Moonbase Laboratory while there are clues on it."
instance HasModifiersFor MoonbaseLaboratory where
  getModifiersFor (MoonbaseLaboratory a) =
    when (a.clues > 0) $ modifySelect a (investigatorAt a.id) [CannotTakeAction $ IsAction Scan]

{- | "Forced - At the end of the round, if there is a [[Starship]] attached to this
location: Place 1 resource on this location (to a maximum of 3). If there are 3
resources on it, spawn the set aside Domaag T'eel enemy at this location."
-}
instance HasAbilities MoonbaseLaboratory where
  getAbilities (MoonbaseLaboratory a) =
    extendRevealed1 a
      -- a [[Starship]] location prints "connected to attached location and vice
      -- versa", so connection is the observable form of its attachment
      $ restricted a 1 (exists $ LocationWithTrait Starship <> connectedTo (be a))
      $ forced
      $ RoundEnds #when

instance RunMessage MoonbaseLaboratory where
  runMessage msg l@(MoonbaseLaboratory attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let resources = Token.countTokens Token.Resource attrs.tokens
      when (resources < 3) $ placeTokens (attrs.ability 1) attrs Token.Resource 1
      when (resources + 1 >= 3) $ createEnemyAt_ Enemies.domaagTeel attrs.id
      pure l
    _ -> MoonbaseLaboratory <$> liftRunMessage msg attrs

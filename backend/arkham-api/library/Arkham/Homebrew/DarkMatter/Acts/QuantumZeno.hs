module Arkham.Homebrew.DarkMatter.Acts.QuantumZeno (quantumZeno) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Elbrus)
import Arkham.Matcher
import Arkham.Token qualified as Token

newtype QuantumZeno = QuantumZeno ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quantumZeno :: ActCard QuantumZeno
quantumZeno = act (2, A) QuantumZeno Cards.quantumZeno Nothing

{- | "[free] Spend 1[per_investigator] clues, as a group: Place a resource token
on your location from the token pool.
Objective - If there are 8 [[Elbrus]] locations in play, and each location has a
resource token on it, you may advance."
-}
instance HasAbilities QuantumZeno where
  getAbilities (QuantumZeno a) =
    [ mkAbility a 1 $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
    , restricted
        a
        2
        ( LocationCount 8 (LocationWithTrait Elbrus)
            <> not_ (exists $ LocationWithTrait Elbrus <> not_ (LocationWithToken Token.Resource))
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage QuantumZeno where
  runMessage msg a@(QuantumZeno attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      placeTokens (attrs.ability 1) lid Token.Resource 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> QuantumZeno <$> liftRunMessage msg attrs

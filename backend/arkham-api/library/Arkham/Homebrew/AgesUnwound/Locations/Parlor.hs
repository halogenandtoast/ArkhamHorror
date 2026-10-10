module Arkham.Homebrew.AgesUnwound.Locations.Parlor (parlor) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMaybe)
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Id
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (metaL)
import Arkham.Matcher

newtype Parlor = Parlor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlor :: LocationCard Parlor
parlor = symbolLabel $ location Parlor Cards.parlor 3 (PerPlayer 1)

{- | "Unengaged enemies at Parlor do not automatically engage investigators who
did not begin the round at Parlor."

'CannotEngage' is the automatic-engagement gate only -- 'Game.CanEngageEnemy'
(the Engage /action/) never reads it -- which is exactly what "do not
automatically engage" asks for.

Who began the round here is recorded in the location's own meta at 'BeginRound'.
It has to be a plain field read rather than a modifier or a scenario-log key:
this runs inside 'HasModifiersFor', and the log is the player-visible scenario
log.
-}
instance HasModifiersFor Parlor where
  getModifiersFor (Parlor a) = do
    let began = toResultDefault [] a.meta :: [InvestigatorId]
    latecomers <- filter (`notElem` began) <$> allInvestigators
    modifySelectMaybe a (UnengagedEnemy <> EnemyAt (be a)) \_ -> do
      guard $ notNull latecomers
      pure $ map CannotEngage latecomers

{- | "Forced - After you reveal Parlor: Spawn 1[per_investigator] copies of The
Myriad Gentleman at Parlor."
-}
instance HasAbilities Parlor where
  getAbilities (Parlor a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage Parlor where
  runMessage msg l@(Parlor attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      lead <- getLead
      spawnMyriadCopiesAt lead n attrs.id
      pure l
    BeginRound -> do
      began <- select $ investigatorAt attrs.id
      attrs' <- liftRunMessage msg attrs
      pure $ Parlor $ attrs' & metaL .~ toJSON began
    _ -> Parlor <$> liftRunMessage msg attrs

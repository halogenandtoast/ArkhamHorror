module Arkham.Homebrew.CircusExMortis.Treacheries.KeepQuiet (keepQuiet) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted hiding (DiscoverClues, EnemyEvaded)

newtype KeepQuiet = KeepQuiet TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keepQuiet :: TreacheryCard KeepQuiet
keepQuiet = treachery KeepQuiet Cards.keepQuiet

instance HasAbilities KeepQuiet where
  getAbilities (KeepQuiet a) =
    [ restricted a 1 InYourThreatArea
        $ forced
        $ oneOf
          [ EnemyDealtDamage #after AnyDamageEffect AnyEnemy (SourceUsedBy You)
          , DiscoverClues #after You Anywhere (atLeast 1)
          ]
    , restricted a 2 InYourThreatArea
        $ freeReaction
        $ EnemyEvaded #after Anyone (EnemyAt YourLocation)
    ]

instance RunMessage KeepQuiet where
  runMessage msg t@(KeepQuiet attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ NearestEnemyTo iid $ NonEliteEnemy <> CanPlaceDoomOnEnemy
      unless (null enemies) $ chooseTargetM iid enemies $ placeDoomOn attrs 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> KeepQuiet <$> liftRunMessage msg attrs

module Arkham.Entity.Some
where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Act ()
import Arkham.Act.Types (Act)
import {-# SOURCE #-} Arkham.Agenda ()
import Arkham.Agenda.Types (Agenda)
import {-# SOURCE #-} Arkham.Asset ()
import Arkham.Asset.Types (Asset)
import Arkham.Campaign ()
import Arkham.Campaign.Types (Campaign)
import Arkham.Classes.Entity
import Arkham.Classes.HasModifiersFor
import {-# SOURCE #-} Arkham.Effect ()
import Arkham.Effect.Types (Effect)
import {-# SOURCE #-} Arkham.Enemy ()
import Arkham.Enemy.Types (Enemy)
import {-# SOURCE #-} Arkham.Event ()
import Arkham.Event.Types (Event)
import {-# SOURCE #-} Arkham.Investigator ()
import Arkham.Investigator.Types (Investigator)
import {-# SOURCE #-} Arkham.Location ()
import Arkham.Location.Types (Location)
import Arkham.Scenario ()
import Arkham.Scenario.Types (Scenario)
import {-# SOURCE #-} Arkham.Skill ()
import Arkham.Skill.Types (Skill)
import {-# SOURCE #-} Arkham.Story ()
import Arkham.Story.Types (Story)
import {-# SOURCE #-} Arkham.Target
import {-# SOURCE #-} Arkham.Treachery ()
import Arkham.Treachery.Types (Treachery)
import Data.Typeable

data SomeEntity where
  SomeEntity
    :: (Entity a, ToJSON a, Show a, Eq a, Typeable a, HasModifiersFor a, Targetable a)
    => SEntity a
    -> a
    -> SomeEntity

instance Eq SomeEntity where
  SomeEntity _ (a :: a) == SomeEntity _ (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

deriving stock instance Show SomeEntity

instance ToJSON SomeEntity where
  toJSON (SomeEntity s e) =
    object ["type" .= show s, "entity" .= e]

instance FromJSON SomeEntity where
  parseJSON = withObject "SomeEntity" $ \o -> do
    s <- o .: "type"
    e <- o .: "entity"
    case s of
      "SLocation" -> SomeEntity SLocation <$> parseJSON e
      "SInvestigator" -> SomeEntity SInvestigator <$> parseJSON e
      "SEnemy" -> SomeEntity SEnemy <$> parseJSON e
      "SAsset" -> SomeEntity SAsset <$> parseJSON e
      "SAct" -> SomeEntity SAct <$> parseJSON e
      "SAgenda" -> SomeEntity SAgenda <$> parseJSON e
      "STreachery" -> SomeEntity STreachery <$> parseJSON e
      "SEvent" -> SomeEntity SEvent <$> parseJSON e
      "SEffect" -> SomeEntity SEffect <$> parseJSON e
      "SSkill" -> SomeEntity SSkill <$> parseJSON e
      "SStory" -> SomeEntity SStory <$> parseJSON e
      "SScenario" -> SomeEntity SScenario <$> parseJSON e
      "SCampaign" -> SomeEntity SCampaign <$> parseJSON e
      _ -> error $ "Unknown SEntity: " <> s

instance Targetable SomeEntity where
  toTarget (SomeEntity _ e) = toTarget e

instance HasModifiersFor SomeEntity where
  getModifiersFor target (SomeEntity _ e) = getModifiersFor target e

data SEntity a where
  SLocation :: SEntity Location
  SInvestigator :: SEntity Investigator
  SEnemy :: SEntity Enemy
  SAsset :: SEntity Asset
  SAct :: SEntity Act
  SAgenda :: SEntity Agenda
  STreachery :: SEntity Treachery
  SEvent :: SEntity Event
  SEffect :: SEntity Effect
  SSkill :: SEntity Skill
  SStory :: SEntity Story
  SScenario :: SEntity Scenario
  SCampaign :: SEntity Campaign

deriving stock instance Show (SEntity a)

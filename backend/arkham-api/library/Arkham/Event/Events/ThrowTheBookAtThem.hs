module Arkham.Event.Events.ThrowTheBookAtThem (throwTheBookAtThem, ThrowTheBookAtThem (..)) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Target
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype Meta = Meta {chosenTome :: Maybe AssetId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ThrowTheBookAtThem = ThrowTheBookAtThem (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throwTheBookAtThem :: EventCard ThrowTheBookAtThem
throwTheBookAtThem = event (ThrowTheBookAtThem . (`with` Meta Nothing)) Cards.throwTheBookAtThem

instance RunMessage ThrowTheBookAtThem where
  runMessage msg e@(ThrowTheBookAtThem (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ assetControlledBy iid <> #tome
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      x <- field AssetCost aid
      sid <- getRandom
      when (x > 0) $ skillTestModifier sid attrs iid (SkillModifier #combat x)
      chooseFightEnemy sid iid attrs
      pure . ThrowTheBookAtThem $ attrs `with` Meta (Just aid)
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> do
          canEvade <- eid <=~> EnemyCanBeEvadedBy (toSource attrs)
          chooseOrRunOneM iid $ cardI18n $ scope "throwTheBookAtThem" do
            when canEvade do
              labeled "automaticallyEvade" $ automaticallyEvadeEnemy iid eid
            labeled "resolveAbilityOnTome"
              $ doStep 1 msg
        _ -> pure ()
      pure e
    DoStep 1 (PassedThisSkillTest iid (isSource attrs -> True)) -> do
      afterSkillTest iid "Throw the Book at Them" do
        push $ ResolveEventChoice iid attrs.id 1 Nothing []
      pure e
    ResolveEventChoice iid eid _ _ _ | eid == attrs.id -> do
      for_ (chosenTome meta) \tome -> do
        -- True Magick (5) is a [Tome], so it is a legal choice, and per the FAQ
        -- (February 2025) resolving an ability "on" it means revealing a [Spell] from
        -- hand and treating True Magick as that asset. Two things that needs:
        --   * real windows. The wrapper re-filters the hand with
        --     `getCanPerformAbility iid ws`, which guards `notNull matching` before any
        --     criteria -- with [] it finds nothing and `chooseOne` throws (#5801). Pass
        --     the full `defaultWindows`: unlike Sign Magick (3) this card allows an
        --     [action] OR [fast] ability, so a borrowed [fast] one is legal here.
        --   * only the wrapper. getTrueMagickInHandAbilities re-sources the in-hand
        --     spells onto True Magick so matchers can see them, but those proxies skip
        --     the reveal and would also list cards that are not in play.
        let notBorrowed ab = case ab.source of
              ProxySource (CardIdSource _) _ -> False
              _ -> True
        abilities <-
          map (doesNotProvokeAttacksOfOpportunity . (`applyAbilityModifiers` [IgnoreActionCost]))
            . filter notBorrowed
            <$> select
              ( AbilityOnAsset (AssetWithId tome)
                  <> oneOf [AbilityIsActionAbility, AbilityIsFastAbility]
                  <> PerformableAbility [IgnoreActionCost]
              )
        when (notNull abilities) do
          chooseOne iid
            $ Label "$label.doNotUseAbility" []
            : [AbilityLabel iid ab (defaultWindows iid) [] [] | ab <- abilities]
      pure e
    _ -> ThrowTheBookAtThem . (`with` meta) <$> liftRunMessage msg attrs

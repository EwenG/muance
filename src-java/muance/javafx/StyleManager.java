package muance.javafx;

import com.sun.javafx.css.SelectorPartitioning;
import com.sun.javafx.css.StyleClassSet;
import javafx.css.*;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.SubScene;
import javafx.scene.layout.Region;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

import static com.sun.javafx.css.StyleManager.loadStylesheet;

public class StyleManager {

  private com.sun.javafx.css.StyleManager styleManager = com.sun.javafx.css.StyleManager.getInstance();
  static private StyleManager instance;

  private Method getSubScene = Node.class.getDeclaredMethod("getSubScene");
  private Method getCacheContainer = com.sun.javafx.css.StyleManager.class.getDeclaredMethod("getCacheContainer", Styleable.class, SubScene.class);
  private Method gatherParentStylesheets = com.sun.javafx.css.StyleManager.class.getDeclaredMethod("gatherParentStylesheets", Parent.class);
  private Method gatherSceneStylesheets = com.sun.javafx.css.StyleManager.class.getDeclaredMethod("gatherSceneStylesheets", Scene.class);
  private Field styleLock = com.sun.javafx.css.StyleManager.class.getDeclaredField("styleLock");
  private Field weakRegionUserAgentStylesheetMap = com.sun.javafx.css.StyleManager.class.getDeclaredField("weakRegionUserAgentStylesheetMap");
  private Class keyClass = Class.forName("com.sun.javafx.css.StyleManager$Key");
  private Field key = com.sun.javafx.css.StyleManager.class.getDeclaredField("key");
  private Field keyClassName = keyClass.getDeclaredField("className");
  private Field keyId = keyClass.getDeclaredField("id");
  private Field keyStyleClasses = keyClass.getDeclaredField("styleClasses");
  private Class CacheContainerClass = Class.forName("com.sun.javafx.css.StyleManager$CacheContainer");
  private Method getCacheMap = CacheContainerClass.getDeclaredMethod("getCacheMap", List.class, String.class);
  private Method getInlineStyleSelector = CacheContainerClass.getDeclaredMethod("getInlineStyleSelector", String.class);
  private Class StylesheetContainerClass = Class.forName("com.sun.javafx.css.StyleManager$StylesheetContainer");
  private Field fname = StylesheetContainerClass.getDeclaredField("fname");
  private Field selectorPartitioning = StylesheetContainerClass.getDeclaredField("selectorPartitioning");
  private Field parentUsers = StylesheetContainerClass.getDeclaredField("parentUsers");
  private Class RefListClass = Class.forName("com.sun.javafx.css.StyleManager$RefList");
  private Method refListAdd = RefListClass.getDeclaredMethod("add", Object.class);
  private Class CacheClass = Class.forName("com.sun.javafx.css.StyleManager$Cache");
  private Field cacheSelectors = CacheClass.getDeclaredField("selectors");

  public static StyleManager getInstance() throws NoSuchMethodException, NoSuchFieldException, ClassNotFoundException {
    if(instance == null) {
      instance = new StyleManager();
    }
    return instance;
  }

  private StyleManager() throws NoSuchMethodException, NoSuchFieldException, ClassNotFoundException {
    getSubScene.setAccessible(true);
    getCacheContainer.setAccessible(true);
    gatherParentStylesheets.setAccessible(true);
    gatherSceneStylesheets.setAccessible(true);
    styleLock.setAccessible(true);
    weakRegionUserAgentStylesheetMap.setAccessible(true);
    key.setAccessible(true);
    keyClassName.setAccessible(true);
    keyId.setAccessible(true);
    keyStyleClasses.setAccessible(true);
    getCacheMap.setAccessible(true);
    getInlineStyleSelector.setAccessible(true);
    fname.setAccessible(true);
    selectorPartitioning.setAccessible(true);
    parentUsers.setAccessible(true);
    refListAdd.setAccessible(true);
    cacheSelectors.setAccessible(true);
  }

  public List<Selector> findMatchingStyles(Node node) throws InvocationTargetException, IllegalAccessException, NoSuchMethodException, InstantiationException {
    final Scene scene = node.getScene();
    final SubScene subScene = (SubScene) getSubScene.invoke(node);
    if (scene == null) {
      return new ArrayList<>();
    }

    Object cacheContainer = getCacheContainer.invoke(styleManager, node, subScene);
    if (cacheContainer == null) {
      assert false : node.toString();
      return new ArrayList<>();
    }

    synchronized (styleLock.get(null)) {
      final Parent parent =
          (node instanceof Parent)
              ? (Parent) node : node.getParent();

      final List<Object> parentStylesheets =
          (List<Object>) gatherParentStylesheets.invoke(styleManager, parent);

      final boolean hasParentStylesheets = parentStylesheets.isEmpty() == false;

      final List<Object> sceneStylesheets = (List<Object>) gatherSceneStylesheets.invoke(styleManager, scene);

      final boolean hasSceneStylesheets = sceneStylesheets.isEmpty() == false;

      final String inlineStyle = node.getStyle();
      final boolean hasInlineStyles = inlineStyle != null && inlineStyle.trim().isEmpty() == false;

      final String sceneUserAgentStylesheet = scene.getUserAgentStylesheet();
      final boolean hasSceneUserAgentStylesheet =
          sceneUserAgentStylesheet != null && sceneUserAgentStylesheet.trim().isEmpty() == false;

      final String subSceneUserAgentStylesheet =
          (subScene != null) ? subScene.getUserAgentStylesheet() : null;
      final boolean hasSubSceneUserAgentStylesheet =
          subSceneUserAgentStylesheet != null && subSceneUserAgentStylesheet.trim().isEmpty() == false;

      String regionUserAgentStylesheet = null;
      // is this node in a region that has its own stylesheet?
      Node region = node;
      while (region != null) {
        if (region instanceof Region) {
          regionUserAgentStylesheet = ((WeakHashMap<Region, String>) weakRegionUserAgentStylesheetMap.get(styleManager)).computeIfAbsent(
              (Region) region, Region::getUserAgentStylesheet);

          if (regionUserAgentStylesheet != null) {
            // We want 'region' to be the node that has the user agent stylesheet.
            // 'region' is used below - look for if (hasRegionUserAgentStylesheet) block
            break;
          }
        }
        region = region.getParent();
      }


      final boolean hasRegionUserAgentStylesheet =
          regionUserAgentStylesheet != null && regionUserAgentStylesheet.trim().isEmpty() == false;

      //
      // Are there any stylesheets at all?
      // If not, then there is nothing to match and the
      // resulting StyleMap is going to end up empty
      //
      if (hasInlineStyles == false
          && hasParentStylesheets == false
          && hasSceneStylesheets == false
          && hasSceneUserAgentStylesheet == false
          && hasSubSceneUserAgentStylesheet == false
          && hasRegionUserAgentStylesheet == false
          && styleManager.platformUserAgentStylesheetContainers.isEmpty()) {
        return new ArrayList<>();
      }

      final String cname = node.getTypeSelector();
      final String id = node.getId();
      final List<String> styleClasses = node.getStyleClass();

      if (key.get(styleManager) == null) {
        key.set(styleManager, keyClass.getConstructor().newInstance());
      }

      keyClassName.set(key.get(styleManager), cname);
      keyId.set(key.get(styleManager), id);

      for(int n=0, nMax=styleClasses.size(); n<nMax; n++) {

        final String styleClass = styleClasses.get(n);
        if (styleClass == null || styleClass.isEmpty()) continue;

        ((Set)keyStyleClasses.get(key.get(styleManager))).add(StyleClassSet.getStyleClass(styleClass));
      }

      Map cacheMap = (Map) getCacheMap.invoke(cacheContainer, parentStylesheets, regionUserAgentStylesheet);
      Object cache = cacheMap.get(key.get(styleManager));

      if (cache != null) {
        // key will be reused, so clear the styleClasses for next use
        ((Set)keyStyleClasses.get(key.get(styleManager))).clear();

      } else {

        // If the cache is null, then we need to create a new Cache and
        // add it to the cache map

        // Construct the list of Selectors that could possibly apply
        final List<Selector> selectorData = new ArrayList<>();

        // User agent stylesheets have lowest precedence and go first
        if (hasSubSceneUserAgentStylesheet || hasSceneUserAgentStylesheet) {

          // if has both, use SubScene
          final String uaFileName = hasSubSceneUserAgentStylesheet ?
              subScene.getUserAgentStylesheet().trim() :
              scene.getUserAgentStylesheet().trim();


          Object container = null;
          for (int n = 0, nMax = styleManager.userAgentStylesheetContainers.size(); n < nMax; n++) {
            container = styleManager.userAgentStylesheetContainers.get(n);
            if (uaFileName.equals(fname.get(container))) {
              break;
            }
            container = null;
          }

          if (container == null) {
            Stylesheet stylesheet = loadStylesheet(uaFileName);
            if (stylesheet != null) {
              stylesheet.setOrigin(StyleOrigin.USER_AGENT);
            }
            container = StylesheetContainerClass.getConstructor(String.class, Stylesheet.class).newInstance(uaFileName, stylesheet);
            ((List) styleManager.userAgentStylesheetContainers).add(container);
          }

          if (selectorPartitioning.get(container) != null) {

            final Parent root = hasSubSceneUserAgentStylesheet ? subScene.getRoot() : scene.getRoot();
            refListAdd.invoke(parentUsers.get(container), root);

            final List<Selector> matchingRules =
                ((SelectorPartitioning) selectorPartitioning.get(container)).match(id, cname, (Set<StyleClass>) keyStyleClasses.get(key.get(styleManager)));
            selectorData.addAll(matchingRules);
          }

        } else if (styleManager.platformUserAgentStylesheetContainers.isEmpty() == false) {
          for (int n = 0, nMax = styleManager.platformUserAgentStylesheetContainers.size(); n < nMax; n++) {
            final Object container = styleManager.platformUserAgentStylesheetContainers.get(n);
            if (container != null && selectorPartitioning.get(container) != null) {
              final List<Selector> matchingRules =
                  ((SelectorPartitioning) selectorPartitioning.get(container)).match(id, cname, (Set<StyleClass>) keyStyleClasses.get(key.get(styleManager)));
              selectorData.addAll(matchingRules);
            }
          }
        }

        if (hasRegionUserAgentStylesheet) {
          // Unfortunate duplication of code from previous block. No time to refactor.
          Object container = null;
          for (int n = 0, nMax = styleManager.userAgentStylesheetContainers.size(); n < nMax; n++) {
            container = styleManager.userAgentStylesheetContainers.get(n);
            if (regionUserAgentStylesheet.equals(fname.get(container))) {
              break;
            }
            container = null;
          }

          if (container == null) {
            Stylesheet stylesheet = loadStylesheet(regionUserAgentStylesheet);
            if (stylesheet != null) {
              stylesheet.setOrigin(StyleOrigin.USER_AGENT);
            }
            container = StylesheetContainerClass.getConstructor(String.class, Stylesheet.class).newInstance(regionUserAgentStylesheet, stylesheet);
            ((List) styleManager.userAgentStylesheetContainers).add(container);
          }

          if (selectorPartitioning.get(container) != null) {

            // Depending on RefList add method not allowing duplicates.
            refListAdd.invoke(parentUsers.get(container), region);

            final List<Selector> matchingRules =
                ((SelectorPartitioning) selectorPartitioning.get(container)).match(id, cname, (Set<StyleClass>) keyStyleClasses.get(key.get(styleManager)));
            selectorData.addAll(matchingRules);
          }

        }

        // Scene stylesheets come next since declarations from
        // parent stylesheets should take precedence.
        if (sceneStylesheets.isEmpty() == false) {
          for (int n = 0, nMax = sceneStylesheets.size(); n < nMax; n++) {
            final Object container = sceneStylesheets.get(n);
            if (container != null && selectorPartitioning.get(container) != null) {
              final List<Selector> matchingRules =
                  ((SelectorPartitioning) selectorPartitioning.get(container)).match(id, cname, (Set<StyleClass>) keyStyleClasses.get(key.get(styleManager)));
              selectorData.addAll(matchingRules);
            }
          }
        }

        // lastly, parent stylesheets
        if (hasParentStylesheets) {
          final int nMax = parentStylesheets == null ? 0 : parentStylesheets.size();
          for (int n = 0; n < nMax; n++) {
            final Object container = parentStylesheets.get(n);
            if (selectorPartitioning.get(container) != null) {
              final List<Selector> matchingRules =
                  ((SelectorPartitioning) selectorPartitioning.get(container)).match(id, cname, (Set<StyleClass>) keyStyleClasses.get(key.get(styleManager)));
              selectorData.addAll(matchingRules);
            }
          }
        }


        // create a new Cache from these selectors.
        cache = CacheClass.getConstructor(List.class).newInstance(selectorData);
        cacheMap.put(key, cache);

        // cause a new Key to be created the next time this method is called
        key = null;
      }

      // cache.getStyleMap(...) logic
      final List<Selector> selectors = new ArrayList<>();

      final List<Selector> stylesheetsSelectors = (List<Selector>) cacheSelectors.get(cache);
      final int stylesheetsSelectorsSize = stylesheetsSelectors.size();

      for(int s = 0; s < stylesheetsSelectorsSize; s++) {
        final Selector sel = stylesheetsSelectors.get(s);
        if(sel.applies(node)) {
          selectors.add(sel);
        }
      }

      if(hasInlineStyles) {
        Selector selector = (Selector) getInlineStyleSelector.invoke(cacheContainer, inlineStyle);
        if(selector != null) {
          selectors.add(selector);
        }
      }

      return selectors;
      }
  }
}

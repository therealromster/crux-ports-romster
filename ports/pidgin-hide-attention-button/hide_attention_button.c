/*
 * Pidgin Plugin - Hide Pidgin Attention Button
 * 
 * Copyright 2008 Craig Harding <craigwharding@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#define PURPLE_PLUGINS

#include <stdio.h>
#include "plugin.h"
#include "version.h"
#include "gtkconv.h"
#include "gtkimhtmltoolbar.h"
#include "gtkplugin.h"
#include "debug.h"

#define PLUGIN_ID     			"gtk-charding-hide-pidgin-att-button"
#define PLUGIN_VERSION			"0.1"
#define PLUGIN_NAME				"Hide Pidgin Attention Button"
#define PLUGIN_AUTHOR			"Craig Harding <craigwharding@gmail.com>"
#define PLUGIN_WEBSITE			""

GtkWidget *get_attention_button(PidginConversation *gtkconv) {
	GList *child, *button, *label;
	GtkIMHtmlToolbar *toolbar;
	GtkWidget *tbb;
	gpointer b;
	GtkContainer *toolbar_hbox;
	GtkLabel *button_label;

	/* There must be a more compact way to do this for just this button? */
	toolbar = GTK_IMHTMLTOOLBAR(gtkconv->toolbar);
	/* lean-view shows the buttons with images and words */	
	toolbar_hbox = g_object_get_data(G_OBJECT(toolbar), "lean-view");
	for (child = gtk_container_get_children(GTK_CONTAINER(toolbar_hbox)); child != NULL; child = child->next) {
		b = child->data;
		if (GTK_IS_BUTTON(b)) {
			for (button = gtk_container_get_children(GTK_CONTAINER(b)); button != NULL; button = button->next) {
				tbb = button->data;
				if (GTK_IS_HBOX(tbb)) {
					// Each hbox has an image and a label
					label = g_list_last(gtk_container_get_children(GTK_CONTAINER(tbb)));
					button_label = label->data;
					if (!g_strcmp0(gtk_label_get_text(button_label), "Attention!")) { 
						purple_debug_info(PLUGIN_ID, "Found attention button with lean-view\n");
						return tbb;
					}
				}
			}
		}
	}
	return NULL;
}

void attention_button_show_signal_cb(GtkWidget *widget, char *signal) {
	purple_debug_info(PLUGIN_ID, "Signal: %s, widget-show signal, hiding attention button\n", signal);
	gtk_widget_hide(widget);
}

/* Takes a conversation window and hides/unhides the Attention! button */
void do_it(gboolean hide, PidginConversation *gtkconv) {

	GtkWidget *attention_button = get_attention_button(gtkconv);
	GtkIMHtmlToolbar *toolbar = GTK_IMHTMLTOOLBAR(gtkconv->toolbar);

	if (hide) gtk_widget_hide(GTK_WIDGET(g_object_get_data(G_OBJECT(toolbar), "attention")));
	else gtk_widget_show_all(GTK_WIDGET(g_object_get_data(G_OBJECT(toolbar), "attention")));
	if (hide) {
		purple_debug_info(PLUGIN_ID, "Hiding attention button\n"); gtk_widget_hide(attention_button);
	}
	else gtk_widget_show_all(attention_button);
}

/* conversation-hiding, conversation-created, conversation-switched */
void conv_created_cb(PurpleConversation *conv, char *signal) {
	purple_debug_info(PLUGIN_ID, "Signal: %s, hiding attention button\n", signal);
	do_it(TRUE, PIDGIN_CONVERSATION(conv));	
	GtkWidget *attention_button = get_attention_button(PIDGIN_CONVERSATION(conv));		
	GtkIMHtmlToolbar *toolbar = GTK_IMHTMLTOOLBAR(PIDGIN_CONVERSATION(conv)->toolbar);
	GtkWidget *wide_att_button = g_object_get_data(G_OBJECT(toolbar), "attention");
	
	purple_debug_info(PLUGIN_ID, "Assigning 'show' signal callback\n");
	g_signal_connect(G_OBJECT(attention_button), "show",
		G_CALLBACK(attention_button_show_signal_cb), "gtkwidget-show-lean");
	g_signal_connect(G_OBJECT(wide_att_button), "show",
				G_CALLBACK(attention_button_show_signal_cb), "gtkwidget-show-wide");
}

static gboolean
plugin_load(PurplePlugin *plugin) {
	GtkWidget *attention_button, *wide_att_button;
	GList *conv;
	GtkIMHtmlToolbar *toolbar;

	purple_debug_info(PLUGIN_ID, "Plugin loaded, hiding all attention buttons\n");
	//purple_signal_connect(pidgin_conversations_get_handle(), "displayed-im-msg", plugin, PURPLE_CALLBACK(pidgin_conv_displayed_cb), "displayed-im-msg");
	//purple_signal_connect(pidgin_conversations_get_handle(), "conversation-hiding", plugin, PURPLE_CALLBACK(conv_created_cb), "conversation-hiding");
	//purple_signal_connect(pidgin_conversations_get_handle(), "conversation-displayed", plugin, PURPLE_CALLBACK(pidgin_conv_displayed_cb), "conversation-displayed");
	purple_signal_connect(purple_conversations_get_handle(), "conversation-created", plugin, PURPLE_CALLBACK(conv_created_cb), "conversation-created");
	//purple_signal_connect(purple_conversations_get_handle(), "conversation-switched", plugin, PURPLE_CALLBACK(conv_created_cb), "conversation-switched");
	// Wait for change to /pidgin/conversations/toolbar/wide 
	//purple_prefs_connect_callback(plugin, "/pidgin/conversations/toolbar/wide", wide_pref_cb, NULL);
	//purple_prefs_connect_callback(plugin, "/pidgin/conversations/show_formatting_toolbar", show_formatting_toolbar_cb, NULL);

	/* Perform hiding on all open conversations and set up 'show' signal */
	for(conv = purple_get_conversations(); conv != NULL; conv = conv->next) {
		do_it(TRUE, PIDGIN_CONVERSATION((PurpleConversation *)conv->data));		
		attention_button = get_attention_button(PIDGIN_CONVERSATION((PurpleConversation *)conv->data));		
		toolbar = GTK_IMHTMLTOOLBAR(PIDGIN_CONVERSATION((PurpleConversation *)conv->data)->toolbar);
		wide_att_button = g_object_get_data(G_OBJECT(toolbar), "attention");
		if (attention_button == NULL) {
			purple_debug_error(PLUGIN_ID, "attention_button is NULL, something wrong\n");
		}
		if (purple_prefs_get_bool("/pidgin/conversations/toolbar/wide"))
			g_signal_connect(G_OBJECT(wide_att_button), "show",
				G_CALLBACK(attention_button_show_signal_cb), "gtkwidget-show-wide");
		else 
			g_signal_connect(G_OBJECT(attention_button), "show",
				G_CALLBACK(attention_button_show_signal_cb), "gtkwidget-show-lean");
	}
	
	return TRUE;
}


static gboolean
plugin_unload(PurplePlugin *plugin) {
	GList *conv;
	GtkWidget *attention_button, *wide_att_button;
	GtkIMHtmlToolbar *toolbar;

	// Disconnect 'show' signal on all attention buttons
	for(conv = purple_get_conversations(); conv != NULL; conv = conv->next) {
		attention_button = get_attention_button(PIDGIN_CONVERSATION((PurpleConversation *)conv->data));		
		toolbar = GTK_IMHTMLTOOLBAR(PIDGIN_CONVERSATION((PurpleConversation *)conv->data)->toolbar);
		wide_att_button = g_object_get_data(G_OBJECT(toolbar), "attention");
		g_signal_handlers_disconnect_by_func (attention_button, G_CALLBACK(attention_button_show_signal_cb), "gtkwidget-show-lean");
		g_signal_handlers_disconnect_by_func (wide_att_button, G_CALLBACK(attention_button_show_signal_cb), "gtkwidget-show-wide");
		// Show all buttons on all conversations
		do_it(FALSE, PIDGIN_CONVERSATION((PurpleConversation *)conv->data));
	}
	purple_prefs_disconnect_by_handle(plugin);
	return TRUE;
}

static PurplePluginInfo info =
{
	PURPLE_PLUGIN_MAGIC,
	PURPLE_MAJOR_VERSION,
	PURPLE_MINOR_VERSION,
	PURPLE_PLUGIN_STANDARD,
	PIDGIN_PLUGIN_TYPE,
	0,
	NULL,
	PURPLE_PRIORITY_DEFAULT,
	PLUGIN_ID,
	PLUGIN_NAME, // Plugin name
	PLUGIN_VERSION,

	"Hides the conversation attention button", // Plugin Summary
	"Hides the attention button", // Long desc
	PLUGIN_AUTHOR,
	PLUGIN_WEBSITE,
	plugin_load,
	plugin_unload,
	NULL, //plugin_destroy
	//&ui_info,
	NULL,
	NULL, // For plugin actions
	NULL, // This and next three NULLs for 'future use'
	NULL
};

static void
init_plugin(PurplePlugin *plugin) {
	
}

PURPLE_INIT_PLUGIN(personalbar, init_plugin, info)
